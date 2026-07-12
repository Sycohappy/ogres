# syntax=docker/dockerfile:1

# -----------------------------------------------------------------------------
# Stage 1: install Node dependencies and build CSS
# -----------------------------------------------------------------------------
FROM node:22-alpine AS node-builder

WORKDIR /build

COPY package.json package-lock.json ./

RUN npm ci

COPY . .

RUN npm rebuild esbuild \
 && mkdir -p web/release \
 && npx esbuild \
      --bundle \
      --minify \
      --loader:.svg=dataurl \
      --outfile=./web/release/ogres.app.css \
      ./src/main/ogres/app/resource/root.css


# -----------------------------------------------------------------------------
# Stage 2: compile the ClojureScript frontend
# -----------------------------------------------------------------------------
FROM clojure:temurin-21-tools-deps-bookworm AS frontend-builder

ARG VERSION=local
ARG CLIENT_SOCKET_URL=ws://127.0.0.1:8080/ws

WORKDIR /build

COPY . .
COPY --from=node-builder /build/node_modules ./node_modules
COPY --from=node-builder /build/web/release/icons.svg ./web/release/icons.svg
COPY --from=node-builder /build/web/release/ogres.app.css ./web/release/ogres.app.css

RUN mkdir -p "web/release/${VERSION}" \
 && cp web/release/icons.svg \
       web/release/ogres.app.css \
       "web/release/${VERSION}/" \
 && cp web/dev/bestiary.md \
       web/dev/mt28.png \
       "web/release/${VERSION}/" 2>/dev/null || true

RUN clojure -M -m shadow.cljs.devtools.cli release app \
    --config-merge \
    "{:closure-defines {ogres.app.const/VERSION \"${VERSION}\" ogres.app.const/PATH \"/release/${VERSION}\" ogres.app.const/SOCKET-URL \"${CLIENT_SOCKET_URL}\"}}" \
 && cp web/release/ogres.app.js \
       "web/release/${VERSION}/ogres.app.js"


# -----------------------------------------------------------------------------
# Stage 3: retrieve the upstream static play shell
# -----------------------------------------------------------------------------
FROM alpine/git:latest AS website

WORKDIR /website

RUN git clone \
      --depth 1 \
      --single-branch \
      --branch gh-pages \
      https://github.com/samcf/ogres.git \
      .


# -----------------------------------------------------------------------------
# Stage 4: final Nginx web image
# -----------------------------------------------------------------------------
FROM nginx:alpine AS final

ARG VERSION=selfhosted

ENV SERVER_SOCKET_URL=http://ogres-backend:8090/ws
ENV RELEASE_VERSION=${VERSION}

# Runtime target used by Nginx when proxying /ws.
ENV SERVER_SOCKET_URL=http://ogres-backend:8090/ws

COPY --from=website /website /usr/share/nginx/html

# Use the fork's customized landing page.
COPY web/index.html /usr/share/nginx/html/index.html

RUN sed -i 's#href="/play"#href="/play?r=local"#g' \
    /usr/share/nginx/html/index.html

# Include the fork's local website content.
COPY site/web /usr/share/nginx/app/site/web

# Include only the compiled immutable release, not the source tree.
COPY --from=frontend-builder \
     /build/web/release/${VERSION} \
     /usr/share/nginx/app/web/release/${VERSION}

# Ogres-generated share links use r=latest. Make that path resolve to the
# self-hosted build rather than falling back to the upstream public release.
RUN ln -s "/usr/share/nginx/app/web/release/${VERSION}" \
          /usr/share/nginx/app/web/release/latest

# Make the self-hosted build the only advertised release. This causes /play
# to select it by default instead of an upstream ogres.app release.
RUN printf '%s\n' "${VERSION}" > /usr/share/nginx/html/releases.txt

# The official Nginx image substitutes SERVER_SOCKET_URL at startup.
COPY deploy/docker-nginx.template \
     /etc/nginx/templates/default.conf.template

EXPOSE 80
