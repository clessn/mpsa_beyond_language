#!/usr/bin/env python3
"""Build the Overleaf version of the manuscript for Social Science Computer Review.

SSCR asks for APA 7 manuscript formatting, so the manuscript uses the apa7
class in manuscript mode (one column, double-spaced, running head, page
numbers), anonymized with the class's `mask` option, with APA 7 references
from biblatex-apa (Overleaf runs biber automatically). The source has one
sentence per line, so co-authors can read and edit it sentence by sentence.

Output: main.tex (anonymized manuscript), title_page.tex (identifying
information), references.bib, figures/.

Usage (from the project root):
    python3 docs/pub/build_overleaf.py <output_dir>

Quarto (docs/pub/pub.qmd) remains the source of truth until the text moves to
Overleaf; after that, a rebuild would overwrite edits made on Overleaf.
"""
import re
import shutil
import subprocess
import sys
from pathlib import Path

PUB = Path(__file__).resolve().parent
ROOT = PUB.parent.parent
OUT = Path(sys.argv[1]).resolve()
NOTE = "XXFIGNOTEXX"

src = (PUB / "pub.qmd").read_text()
yaml_end = src.index("\n---", 4)
yaml, body = src[:yaml_end], src[yaml_end + 4:]

def yaml_block(key):
    m = re.search(rf"^{key}: \|\n((?:  .*\n?)+)", yaml, re.M)
    return " ".join(l.strip() for l in m.group(1).splitlines()) if m else ""

title = re.search(r'^title: "(.*)"', yaml, re.M).group(1)
subtitle = re.search(r'^subtitle: "(.*)"', yaml, re.M).group(1)
shorttitle = re.search(r'^shorttitle: "(.*)"', yaml, re.M).group(1)
abstract = yaml_block("abstract")
keywords = re.findall(r"^  - (.+)$", yaml[yaml.index("keywords:"):yaml.index("date:")], re.M)

# apaquarto figure notes (apa-note="...") are not understood by plain LaTeX
# output: carry each note in the caption behind a marker, then move it to
# \figurenote{} under the figure, as APA style places it.
def fig(m):
    cap, path, attrs = m.group(1), m.group(2), m.group(3)
    note = re.search(r'apa-note="(.*)"[ \t]*$', attrs)
    ident = re.search(r"#(fig-[\w-]+)", attrs).group(1)
    cap = cap + (f" {NOTE} " + note.group(1) if note else "")
    return f"![{cap}]({path}){{#{ident}}}"
body = re.sub(r"^!\[([^\]]*)\]\(([^)]*)\)\{(#fig-.*)\}[ \t]*$", fig, body, flags=re.M)

tmp = PUB / "_overleaf_body.qmd"
tmp.write_text(
    "---\ntitle: x\nbibliography: references.bib\ncite-method: biblatex\nmask: true\n"
    "format:\n  latex:\n    keep-tex: true\n    wrap: none\n---\n" + body)
try:
    subprocess.run(["quarto", "render", tmp.name, "--to", "latex"], cwd=PUB, check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, timeout=300)
    tex = (PUB / "_overleaf_body.tex").read_text()
finally:
    for f in PUB.glob("_overleaf_body*"):
        f.unlink()

doc = tex[tex.index("\\begin{document}") + len("\\begin{document}"):tex.index("\\end{document}")]
doc = re.sub(r"\\maketitle\s*", "", doc)
doc = re.sub(r"\\printbibliography(\[[^\]]*\])?\s*", "", doc)
doc = re.sub(r"\\section\*?\{References\}.*?(?=\\section|\Z)", "\\\\printbibliography\n\n", doc, flags=re.S)

# Figures: caption above (APA), note below as \figurenote
def figure(m):
    f = m.group(0)
    cap = re.search(r"^\\caption\{(\\label\{[^}]*\})(.*)\}[ \t]*$", f, re.M)
    if not cap:
        return f
    label, text = cap.group(1), cap.group(2)
    note = ""
    if NOTE in text:
        text, note = [x.strip() for x in text.split(NOTE, 1)]
    graphic = re.search(r"\\pandocbounded\{\\includegraphics\[[^\]]*\]\{([^}]*)\}\}", f).group(1)
    return ("\\begin{figure}[htbp]\n\\caption{" + text + "}" + label + "\n"
            "\\includegraphics[width=\\linewidth]{" + graphic + "}\n"
            + ("\\figurenote{" + note + "}\n" if note else "") + "\\end{figure}")
doc = re.sub(r"\\begin\{figure\}.*?\\end\{figure\}", figure, doc, flags=re.S)

# Tables: Pandoc's longtable becomes a plain table float with booktabs rules
def longtable_to_table(m):
    t = m.group(0)
    cap = re.search(r"\\caption\{(.*?)\}(\\label\{[^}]*\})\\tabularnewline", t, re.S)
    head = t[t.index("\\toprule\\noalign{}") + len("\\toprule\\noalign{}"):t.index("\\midrule\\noalign{}")]
    head = re.sub(r"\\begin\{minipage\}\[b\]\{\\linewidth\}\\raggedright|\\begin\{minipage\}\[b\]\{\\linewidth\}\\raggedleft|\\end\{minipage\}", "", head)
    head = " ".join(head.split())
    rows = t[t.index("\\endlastfoot") + len("\\endlastfoot"):t.index("\\end{longtable}")].strip()
    align = "l" + "r" * head.count("&")
    caption = (f"\\caption{{{' '.join(cap.group(1).split())}}}{cap.group(2)}\n" if cap else "")
    return ("\\begin{table}[htbp]\n" + caption + "\\centering\n\\begin{tabular}{" + align + "}\n\\toprule\n"
            + head + "\n\\midrule\n" + rows + "\n\\bottomrule\n\\end{tabular}\n\\end{table}")
doc = re.sub(r"\\begin\{longtable\}.*?\\end\{longtable\}", longtable_to_table, doc, flags=re.S)

# One sentence per line, in running text only (not inside floats)
ABBREV = r"(?<!\bvs)(?<!\be\.g)(?<!\bi\.e)(?<!\bal)(?<!\bp)(?<!\bpp)(?<!\bcf)(?<!\bNo)(?<!\bFig)(?<!\bU\.S)(?<!\betc)(?<!\bapprox)"
SPLIT = re.compile(ABBREV + r"([.?!][)\"”’']*)[ ]+(?=[A-Z(\\\"“‘])")
def one_sentence_per_line(text):
    out, in_float = [], 0
    for line in text.split("\n"):
        if re.match(r"\\begin\{(figure|table|tabular)", line):
            in_float += 1
        if in_float == 0 and not line.startswith("\\"):
            line = SPLIT.sub(r"\1\n", line)
        if re.match(r"\\end\{(figure|table|tabular)", line):
            in_float -= 1
        out.append(line)
    return "\n".join(out)
doc = one_sentence_per_line(doc)
abstract_lines = SPLIT.sub(r"\1\n", abstract)

(OUT / "figures").mkdir(parents=True, exist_ok=True)
for p in sorted(set(re.findall(r"\.\./\.\./results/graphs/([\w.-]+\.png)", doc))):
    shutil.copy(ROOT / "results" / "graphs" / p, OUT / "figures" / p)
doc = doc.replace("../../results/graphs/", "figures/")
shutil.copy(PUB / "references.bib", OUT / "references.bib")
for stale in ("sagej.cls",):
    (OUT / stale).unlink(missing_ok=True)

def esc(s):
    return s.replace("&", "\\&").replace("%", "\\%").replace("’", "'")

main = r"""% Manuscript for Social Science Computer Review, in APA 7 manuscript format
% (apa7 class: one column, double-spaced). Anonymized for review with the
% `mask` option; identifying information is in title_page.tex.
% References: APA 7 with biblatex-apa (Overleaf runs biber automatically).
% One sentence per line.
\documentclass[man,12pt,floatsintext,mask,donotrepeattitle]{apa7}

\usepackage[american]{babel}
\usepackage[T1]{fontenc}
\usepackage{newtxtext,newtxmath}
\usepackage{csquotes}
\usepackage[style=apa,backend=biber]{biblatex}
\addbibresource{references.bib}
\usepackage{booktabs,array,calc}
\usepackage{graphicx}
\usepackage[section]{placeins}
\usepackage{newunicodechar}
\newunicodechar{→}{\ensuremath{\rightarrow}}
\newunicodechar{−}{\ensuremath{-}}
\newunicodechar{≈}{\ensuremath{\approx}}
\newunicodechar{±}{\ensuremath{\pm}}
\providecommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}

\title{""" + esc(title) + r""": """ + esc(subtitle) + r"""}
\shorttitle{""" + esc(shorttitle) + r"""}
\authorsnames{Anonymized for review}
\authorsaffiliations{Anonymized for review}

\abstract{
""" + esc(abstract_lines) + r"""
}
\keywords{""" + ", ".join(keywords) + r"""}

\begin{document}
\maketitle
""" + doc + r"""
\end{document}
"""
(OUT / "main.tex").write_text(main)

# Separate title page with the identifying information (not sent to reviewers)
subprocess.run(["quarto", "pandoc", "title_page.qmd", "-f", "markdown", "-s", "-o", str(OUT / "title_page.tex"),
                "--wrap=none", "-V", "documentclass=article", "-V", "papersize=letter",
                "-V", "fontsize=12pt", "-V", "geometry=margin=1in"], cwd=PUB, check=True)
print("Wrote", OUT / "main.tex", "and", OUT / "title_page.tex")
