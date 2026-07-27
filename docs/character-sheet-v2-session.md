# Character Sheet V2 — Session Handoff

**Date:** 2026-07-27 (updated)  
**Branch:** `feature/multi-damage-buttons`  
**Purpose:** Preserve context from the Cursor session so work can continue in a new workspace.  
**Dev host:** Cursor remote → `hl-ubuntu` (`10.0.0.29`); local test via Docker Compose; K3s deploy is a later step.

---

## Where we left off

1. **Shipped on this branch:** multi-damage chat buttons (tested locally via Docker).
2. **Planned next (not implemented):** Character Sheet V2 — structured JSON, resources/rest, Roll20 import rewrite, combat durations, **Roll20-class play UI**, **5etools JSON catalog**.
3. **Reference fixtures:**
   - [`character_sheets/Argamon_flamebound.json`](../character_sheets/Argamon_flamebound.json) — hand-authored v1-style sheet (incomplete vs Roll20).
   - [`character_sheets/Argamon Flamebound _ Roll20 Characters.pdf`](../character_sheets/Argamon%20Flamebound%20_%20Roll20%20Characters.pdf) — full Roll20 export (source of truth for gaps).
4. **UX target (screenshots):** Roll20 dark dashboard for Argamon — 3-column layout, Combat/Spells/Inventory/Features tabs, HP/rest widgets, action/bonus/reaction lists with resource checkboxes and pools (e.g. Lay on Hands `15/25`). Current Ogres sheet is a light bestiary-style page (prose + some EDN leaking into AC/speed).

**Pickup order:** Phase 1 (schema + structured rolls) + Phase 3 fixture-driven Roll20 import early; then Phase 2 resources/rest; Phase 4 durations; Phase 5 Roll20-class UI; Phase 6 5etools catalog adapter.

---

## Session summary (chat so far)

### 1. How damage works today (v1)

Damage is **not** live combat math against HP. It is a two-step chat dice flow:

1. Click a sheet action → optional **to-hit** (`d20 + modifier` parsed from prose).
2. Click **Damage** in chat → roll parenthesized dice like `(2d6 + 7)` from that message text.

Core logic: `src/main/ogres/app/initiative.cljs`.  
Stats, resistance, crits, and weapons are **not** recalculated at roll time — only text already on the action.

### 2. Warhammer +1 problem

Argamon’s Warhammer lists two damages (1d8+4 one-hand **or** 1d10+4 two-hand). The old Damage button parsed **both** expressions and **summed** them. The word “or” was ignored.

### 3. Feature shipped: multi-damage buttons

On `feature/multi-damage-buttons`:

- Each `(NdM + K)` gets its own chat button (labels like `1d8+4 bludgeoning`).
- Clicking a button rolls **only** that expression.
- Single-damage actions still show one **Damage** button.
- Files touched: `initiative.cljs`, `panel_chat.cljs`, `panel_chat.css`, `character_sheet_test.cljs`.
- Also fixed local Docker nginx by setting `RELEASE_VERSION` / `SERVER_SOCKET_URL` in `docker-compose.yaml`.

**Local test:** `docker compose up` → http://127.0.0.1:8080/play?r=dev (or `http://10.0.0.29:8080/play?r=dev` from LAN)  
Note: sheet pop-out is empty inside Cursor’s embedded browser; use a real browser.

### 4. Decision: move to structured sheet JSON (V2)

Agreed approach:

- **Scope (first pass focus):** structured attack / save / damage (and then full PC model).
- **Compatibility:** **dual-read** — prefer structured fields when present; fall back to prose scanning for legacy sheets.
- **Ground-up PC sheet:** not just attacks — resources like Roll20 (spell slots, long rest, durations), and a serious Roll20 import rewrite. Argamon PDF shows the current JSON is missing a lot.

### 5. Design: derived vs fundamental values

Use **layers**, not a single “store the final number” approach:

| Layer | Examples |
|--------|----------|
| **Fundamentals** | Ability **base** scores, class/level, HP roll history / override |
| **Persistent modifiers** | ASI, Amulet of Health (`set: 19` while attuned), permanent item bonuses |
| **Derived** | Effective scores, mods, natural max HP, spell DC |
| **Runtime overlays** | Aid max-HP bump, temp HP, spent slots, active effects |

- Don’t bake Amulet into the only CON value — base + modifier with source.
- Temporary spell effects belong in `runtime.effects`, not permanent vitals.
- Imports may use `maxOverride` until full level history exists.

### 6. Design: catalog vs listing everything on the sheet

**Hybrid + 5etools data:**

- **Catalog source:** 5etools JSON corpus (classes, subclasses, class features, species, feats, spells, items, etc.) — the data files, not the 5e.tools UI.
- **Sheet stores resolved copies** of features/resources/attacks/items with optional `source` links back to catalog IDs (e.g. `5etools:classFeature:paladin|Lay on Hands|PHB`).
- Play/import/homebrew use the **sheet** as source of truth; catalog is for browse / apply / sync / level-up grants — not live-only rendering during combat.
- Pure catalog-only at roll time is too heavy and couples play to an external tree; pure sheet-only works short-term but hurts level-up consistency.

**Storage / legal posture:**

- Keep full 5etools `data/` **private** on hl-ubuntu or a cluster volume — **do not commit** non-SRD WotC text into the public Ogres git repo or public GHCR images.
- Optional: also self-host the 5e.tools UI as a **LAN-only companion** for browsing; Ogres only needs the JSON.
- Adapter maps 5etools records → Ogres v2 feature / resource / attack / item / spell shapes.
- SRD-only subset is fine later if anything must be redistributed; personal homelab can use the full private corpus.

### 7. Design: do we need a new DB?

**No new server DB for now.**

- Sheets already persist for the **host** via **DataScript → IndexedDB** (`provider/state.cljs`, `provider/idb.cljs`).
- Put v2 `runtime` inside `:character-sheet/data` so spent slots/HP persist automatically.
- **Catalog** = private 5etools JSON on disk (or a small curated EDN/JSON subset shipped with the app for demos).
- Add Postgres/cloud only if you need cross-device sync or accounts later.
- Export/import JSON remains a good backup story.

### 8. Decision: Roll20-class sheet UX (look + play)

Compared Roll20 Argamon screenshots vs current Ogres popout:

| | Roll20 | Ogres today |
|---|--------|-------------|
| Look | Dark dashboard, 3 columns, tabs | Light bestiary / prose page |
| Play | Click rolls, HP/rest, resource boxes | Chat damage buttons; little tracking |

**Target:** “Roll20-class for play,” not a pixel-perfect clone.

**Must match (table use):**

- Dark themed play sheet (popout / editor)
- Ability cards + skill list with proficiency markers and click-to-roll
- Center tabs: Combat (first), then Spells / Inventory / Features & Traits / Notes
- Attacks table: range, hit/DC, damage options (multi-damage already started)
- Action / bonus / reaction / free lists with descriptions
- Resource UI: checkbox uses (Channel Divinity) + numeric pools (Lay on Hands)
- HP current/max/temp, damage/heal, hit dice, short/long rest
- AC, speed, defenses, senses, proficiencies/languages

**Defer (do not block V2 core):**

- Exact Roll20 chrome / every gear-icon settings panel
- Full spell-prep UX parity, encumbrance, global adv/disadv/query modes
- Pixel-identical layout

**Visual references:** user screenshots of Roll20 Combat + Actions panels (2026-07-27); Argamon is the golden character.

---

## Roll20 import gaps (current parser)

`parse-roll20` keeps roughly: name, class→type, species, alignment, AC, HP max, initiative, speed, ability scores, passive Perception, languages, resistances, attack/action **prose**.

**Dropped / unused from Argamon PDF:** skills, saving throws, proficiency bonus, spell slots & prepared spells, class/species/feats as structured features, inventory/currency/attunement, current/temp HP, hit dice remaining, death saves, inspiration, weapon masteries, free actions, background/subclass cleanly, bonus/reaction as first-class keys (folded into `:action` with suffixes).

No live resource tracking anywhere today (slots, Lay on Hands pool, concentration, duration).

---

## Character Sheet V2 plan

### Goals

1. Versioned key-value sheet (`version: 2`) — rollable data explicit.
2. Resources: spell slots, feature pools/uses, HP current/temp, hit dice — short/long rest (+ dawn).
3. Combat effects: durations + concentration during initiative.
4. Rewrite Roll20 PC PDF import → v2 (Argamon PDF as golden fixture).
5. Dual-read: structured preferred, prose fallback for v1.
6. **Roll20-class play UI** for PC sheets (see §8) — data first, then skin.
7. **5etools JSON catalog** (private) + adapter to apply class features / items / spells onto the sheet.

Monster/bestiary markdown stays a lighter stat-block subset; PC sheets are the full model.

### Proposed v2 shape (sketch)

Keep DataScript `:character-sheet/data` as an opaque map. Example shape:

```json
{
  "version": 2,
  "identity": {
    "name": "Argamon Flamebound",
    "background": "Noble Crusader",
    "class": "Paladin",
    "subclass": null,
    "species": "Dragonborn - Red",
    "level": 6,
    "xp": { "current": 20475, "next": 23000 },
    "alignment": ["L", "E"],
    "size": "M"
  },
  "vitals": {
    "ac": { "value": 21, "from": ["Plate of Knight's Fellowship", "Shield +1"] },
    "hp": { "max": 64, "average": 64, "formula": "6d10+30" },
    "speed": { "walk": 30 },
    "initiative": { "bonus": 1 },
    "proficiencyBonus": 3,
    "passivePerception": 10,
    "senses": ["Darkvision 60 ft."]
  },
  "abilities": {
    "str": { "score": 17, "save": 6, "proficient": false },
    "cha": { "score": 16, "save": 9, "proficient": true }
  },
  "skills": {
    "athletics": { "modifier": 6, "proficient": true, "ability": "str" }
  },
  "attacks": [
    {
      "id": "warhammer-1",
      "name": "Warhammer +1",
      "kind": "attack",
      "bonus": 7,
      "damage": [
        { "id": "1h", "label": "one-handed", "count": 1, "sides": 8, "modifier": 4, "type": "bludgeoning" },
        { "id": "2h", "label": "two-handed", "count": 1, "sides": 10, "modifier": 4, "type": "bludgeoning" }
      ]
    }
  ],
  "features": [
    {
      "id": "lay-on-hands",
      "name": "Lay on Hands",
      "economy": "bonus",
      "source": "5etools:classFeature:paladin|Lay on Hands|PHB",
      "resourceId": "lay-on-hands"
    }
  ],
  "resources": [
    { "id": "lay-on-hands", "name": "Lay on Hands", "kind": "pool", "max": 25, "recharge": "long-rest" }
  ],
  "spellcasting": [],
  "inventory": {},
  "runtime": {
    "hp": { "current": 64, "temp": 0 },
    "slotsExpended": {},
    "resourceSpent": {},
    "effects": []
  }
}
```

**Rules:**

- Definition caps on `resources` / `spellcasting[].slots`; spent values under `runtime`.
- Attacks first-class; damage options array → one chat button each.
- Features carry `economy` (`action` | `bonus` | `reaction` | `free` | `trait`) for Roll20-like sectioning.
- Catalog apply writes resolved copies + `source` id; play reads the sheet only.
- Recharge: `long-rest` | `short-rest` | `dawn` | `manual`.
- Later: ability **base** + modifiers for items like Amulet of Health; derived effective scores/max HP.

### Phases

| Phase | Deliverable | Status |
|-------|-------------|--------|
| **1** | v2 schema, `character_sheet.cljs` adapter, structured rolls + v1 fallback, convert Argamon sample | **done** |
| **2** | Spend resource/slot events, short/long rest, counters UI, sheet→token HP sync | **done** |
| **3** | `parse-roll20-v2`, Argamon PDF fixture tests, stop merging bonus/reaction into action | **done** |
| **4** | `runtime.effects`, concentration, initiative round expiry | **done** |
| **5** | Roll20-class play UI: dark layout, ability/skill rolls, Combat tab (attacks + action economy + resources), HP/rest widgets; then Spells / Inventory / Features tabs | **done** |
| **6** | Private 5etools catalog import + adapter + apply-to-sheet UX; docs for LAN companion | **done** |

Phases 1–3 remain the critical path. Phase 5 can start once structured attacks/resources exist (after 1–2). Phase 6 can begin in parallel after Phase 1 schema is stable (adapter targets v2 shapes).

### Out of scope (for now)

- Full 5e auto-derivation engine from ability+PB alone  
- Vendoring full non-SRD 5etools data into the public git repo / public images  
- Embedding / merging the 5e.tools **site UI** into Ogres (companion only)  
- Foundry/Plutonium-depth live bi-directional sync with 5etools  
- Pixel-perfect Roll20 clone; global adv/disadv/query parity  
- Complex multiclass beyond one `spellcasting[]` entry  
- Replacing monster markdown with full PC v2  

### Primary files to touch

| Area | Files |
|------|--------|
| Schema / adapter | new `src/main/ogres/app/character_sheet.cljs`, sample JSON |
| Rolls / chat | `initiative.cljs`, `panel_characters.cljs`, `panel_chat.cljs` |
| Events | `events.cljs` |
| Import | `import/parser.cljs`, `import_test.cljs`, Argamon PDF |
| Editor / play UI | `character_sheet_editor.cljs`, `panel_characters.cljs`, related CSS |
| Catalog (Phase 6) | new catalog loader + `5etools` → v2 mapper; config path to private `data/` |
| Persistence (existing) | DataScript + IndexedDB — no new DB |

### Catalog layout (Phase 6 sketch)

```text
# private on hl-ubuntu (example) — not in ogres git
~/data/5etools/data/class/...
~/data/5etools/data/spells/...
~/data/5etools/data/items.json
...
```

Ogres reads via configured path / volume. Optional: `~/services/5etools` Docker companion for browsing only.

---

## How to resume in a new workspace

```sh
cd ~/services/ogres   # on hl-ubuntu
git checkout feature/multi-damage-buttons
git pull
```

1. Read this file.  
2. Start **Phase 1**: define v2 schema + adapter; keep multi-damage button behavior but feed it from structured `attacks[].damage` when present.  
3. Use Argamon PDF + JSON as fixtures for Phase 3; use Roll20 screenshots as Phase 5 UX target.  
4. Local app: `docker compose up -d` → http://127.0.0.1:8080/play?r=dev (ensure `RELEASE_VERSION=dev` in compose for nginx).  
5. Do **not** start Phase 6 by committing 5etools data into the repo — mount privately when ready.

### Related Cursor plan files (local machine)

- `~/.cursor/plans/character_sheet_v2_65573a9d.plan.md`  
- `~/.cursor/plans/character_sheet_v2_9cea49c1.plan.md`  

This repo doc is the portable source of truth for handoff.
