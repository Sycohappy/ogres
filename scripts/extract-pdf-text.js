const fs = require("fs");
const path = require("path");
const pdfjsLib = require("pdfjs-dist/legacy/build/pdf.js");

const file = process.argv[2];
if (!file) {
  console.error("usage: node scripts/extract-pdf-text.js <pdf>");
  process.exit(1);
}

(async () => {
  const data = new Uint8Array(fs.readFileSync(file));
  const pdf = await pdfjsLib.getDocument({ data, disableWorker: true }).promise;
  const lines = [];
  for (let p = 1; p <= pdf.numPages; p++) {
    const page = await pdf.getPage(p);
    const content = await page.getTextContent();
    const pageText = content.items.map((item) => item.str || "").join("\n");
    lines.push(`--- PAGE ${p} ---`);
    lines.push(pageText);
  }
  const out = lines.join("\n");
  const outPath = path.join(
    __dirname,
    "flambel-extract.txt"
  );
  fs.writeFileSync(outPath, out, "utf8");
  console.log("pages", pdf.numPages, "chars", out.length, "written", outPath);
  console.log(out.slice(0, 5000));
})().catch((err) => {
  console.error(err);
  process.exit(1);
});
