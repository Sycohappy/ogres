# Private 5etools data (Phase 6)

Keep full 5etools JSON **out of git**. On hl-ubuntu:

```sh
mkdir -p ~/data/5etools
# Clone or copy only the `data/` tree from a local 5etools mirror you already own access to.
# Example layout:
#   ~/data/5etools/data/class/...
#   ~/data/5etools/data/spells/spells-phb.json
#   ~/data/5etools/data/spells/spells-xge.json
#   ~/data/5etools/data/items.json
```

## Using the catalog in Ogres

1. Open Characters panel → **Import catalog JSON**.
2. Select one or more 5etools-shaped JSON files (objects with `classFeature` / `item` / `spell` / `race` arrays). Spell books are typically under `data/spells/spells-*.json`.
3. Additional imports **merge** into the in-memory catalog (same source id replaces). Use **Clear catalog** to start over.
4. Open a sheet in the editor, search the catalog, click **Apply to open sheet**.

### Spells

Applying a spell:

- Upserts full details into the sheet `:spellIndex` (school, time, range, components, duration, concentration/ritual, entries).
- Appends the spell **name** to `spellcasting[0].prepared[level]` (deduped).
- The Spells tab renders expandable details from `:spellIndex` when present.

Catalog entries are stored in browser memory only for the session.

## Optional LAN companion UI

Self-host the 5e.tools site separately (Docker/nginx) for browsing. Do **not** embed it in Ogres or publish non-SRD data in GHCR images.
