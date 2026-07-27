# Private 5etools data (Phase 6)

Keep full 5etools JSON **out of git**. On hl-ubuntu:

```sh
mkdir -p ~/data/5etools
# Clone or copy only the `data/` tree from a local 5etools mirror you already own access to.
# Example layout:
#   ~/data/5etools/data/class/...
#   ~/data/5etools/data/spells/...
#   ~/data/5etools/data/items.json
```

## Using the catalog in Ogres

1. Open Characters panel → **Import catalog JSON**.
2. Select a 5etools-shaped JSON file (object with `classFeature` / `item` / `spell` / `race` arrays).
3. Open a sheet in the editor, search the catalog, click **Apply to open sheet**.

Catalog entries are stored in browser memory only for the session.

## Optional LAN companion UI

Self-host the 5e.tools site separately (Docker/nginx) for browsing. Do **not** embed it in Ogres or publish non-SRD data in GHCR images.
