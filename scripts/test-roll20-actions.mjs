import fs from "fs";
import pdfjs from "pdfjs-dist/legacy/build/pdf.js";

pdfjs.GlobalWorkerOptions.workerSrc = new URL(
  "../node_modules/pdfjs-dist/legacy/build/pdf.worker.js",
  import.meta.url
).pathname.replace(/^\/([A-Z]:)/, "$1");

function normalizePdfText(text) {
  return text
    .replace(/\r\n/g, "\n")
    .replace(/\r/g, "\n")
    .replace(/\n[ \t]*\n+/g, "\n")
    .replace(/[ \t]*\n[ \t]*/g, "\n")
    .trim();
}

function sectionBody(text, header, endPattern) {
  const re = new RegExp(
    `${header}\\s*\\nNAME[^\\n]*\\n([\\s\\S]*?)(?=\\n(?:${endPattern})|$)`,
    "is"
  );
  const m = text.match(re);
  return m ? m[1].split("\n") : [];
}

const file =
  process.argv[2] ||
  "C:/Users/bry25/Downloads/Argamon Flamebound _ Roll20 Characters.pdf";
const data = new Uint8Array(fs.readFileSync(file));
const pdf = await pdfjs.getDocument({ data }).promise;
const texts = [];
for (let p = 1; p <= pdf.numPages; p++) {
  const page = await pdf.getPage(p);
  const content = await page.getTextContent();
  texts.push(content.items.map((i) => i.str).join("\n"));
}
const text = normalizePdfText(texts.join("\n"));
const attacks = sectionBody(
  text,
  "ATTACKS",
  "WEAPON MASTERIES|ACTIONS|BONUS ACTIONS|DEFENSES|COMBAT REFERENCE"
);
const weapons = sectionBody(
  text,
  "WEAPONS & DAMAGE CANTRIPS",
  "CLASS FEATURES|SPECIES TRAITS|FEATS|CHA\\s"
);
const actions = sectionBody(text, "ACTIONS", "BONUS ACTIONS|REACTIONS|FREE ACTIONS|SPELLS");
console.log("attacks lines", attacks.length);
console.log("weapons lines", weapons.length);
console.log("actions lines", actions.length);
console.log(
  "attack names",
  attacks.filter((l) => /^[A-Z]/.test(l.trim()) && l.trim().length < 40).slice(0, 20)
);
