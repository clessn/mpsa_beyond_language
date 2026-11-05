#!/usr/bin/env python3
"""Build the Overleaf version of the manuscript in the SAGE template.

Converts docs/pub/pub.qmd (anonymized, mask: true) into LaTeX with Quarto,
wraps the body in SAGE's journal class (sagej.cls) with APA 7 references
(biblatex-apa), and writes the result to an output folder ready to be pushed
to Overleaf: main.tex, references.bib, figures/, sagej.cls.

Usage (from the project root):
    python3 docs/pub/build_overleaf.py <output_dir> <path/to/sagej.cls>

Quarto remains the source of truth until the text moves to Overleaf; after
that, edits made on Overleaf would be overwritten by a rebuild.
"""
import re
import shutil
import subprocess
import sys
from pathlib import Path

PUB = Path(__file__).resolve().parent
ROOT = PUB.parent.parent
OUT = Path(sys.argv[1]).resolve()
SAGEJ = Path(sys.argv[2]).resolve()

src = (PUB / "pub.qmd").read_text()
yaml_end = src.index("\n---", 4)
yaml, body = src[:yaml_end], src[yaml_end + 4:]

def yaml_block(key):
    m = re.search(rf"^{key}: \|\n((?:  .*\n?)+)", yaml, re.M)
    return " ".join(l.strip() for l in m.group(1).splitlines()) if m else ""

title = re.search(r'^title: "(.*)"', yaml, re.M).group(1)
subtitle = re.search(r'^subtitle: "(.*)"', yaml, re.M).group(1)
abstract = yaml_block("abstract")
keywords = re.findall(r"^  - (.+)$", yaml[yaml.index("keywords:"):yaml.index("date:")], re.M)

# apaquarto figure notes (apa-note="...") are not understood by plain LaTeX
# output: append each note to its caption.
def fig(m):
    cap, path, attrs = m.group(1), m.group(2), m.group(3)
    note = re.search(r'apa-note="(.*)"[ \t]*$', attrs)
    ident = re.search(r"#(fig-[\w-]+)", attrs).group(1)
    cap = cap + (". *Note.* " + note.group(1) if note else "")
    return f"![{cap}]({path}){{#{ident}}}"
body = re.sub(r"^!\[([^\]]*)\]\(([^)]*)\)\{(#fig-.*)\}[ \t]*$", fig, body, flags=re.M)

tmp = PUB / "_overleaf_body.qmd"
tmp.write_text(
    "---\ntitle: x\nbibliography: references.bib\ncite-method: biblatex\nmask: true\n"
    "format:\n  latex:\n    keep-tex: true\n---\n" + body)
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
# references section heading written by Quarto is replaced by \printbibliography
doc = re.sub(r"\\section\*?\{References\}.*?(?=\\section|\Z)", "\\\\printbibliography\n\n", doc, flags=re.S)

# sagej sets the text in two columns: longtable is single-column only, so each
# Pandoc longtable becomes a full-width table* float; figures become figure*.
def longtable_to_table(m):
    t = m.group(0)
    cap = re.search(r"\\caption\{(.*?)\}(\\label\{[^}]*\})\\tabularnewline", t, re.S)
    head = t[t.index("\\toprule\\noalign{}") + len("\\toprule\\noalign{}"):t.index("\\midrule\\noalign{}")]
    head = re.sub(r"\\begin\{minipage\}\[b\]\{\\linewidth\}\\raggedright|\\begin\{minipage\}\[b\]\{\\linewidth\}\\raggedleft|\\end\{minipage\}", "", head)
    head = " ".join(head.split())
    rows = t[t.index("\\endlastfoot") + len("\\endlastfoot"):t.index("\\end{longtable}")].strip()
    ncol = head.count("&") + 1
    align = "l" + "r" * (ncol - 1)
    caption = (f"\\caption{{{' '.join(cap.group(1).split())}}}{cap.group(2)}\n" if cap else "")
    return ("\\begin{table*}[t]\n" + caption + "\\centering\n\\begin{tabular}{" + align + "}\n\\toprule\n"
            + head + "\n\\midrule\n" + rows + "\n\\bottomrule\n\\end{tabular}\n\\end{table*}")
doc = re.sub(r"\\begin\{longtable\}.*?\\end\{longtable\}", longtable_to_table, doc, flags=re.S)
doc = doc.replace("\\begin{figure}", "\\begin{figure*}").replace("\\end{figure}", "\\end{figure*}")

# figures: copy and point to figures/
(OUT / "figures").mkdir(parents=True, exist_ok=True)
for p in sorted(set(re.findall(r"\.\./\.\./results/graphs/([\w.-]+\.png)", doc))):
    shutil.copy(ROOT / "results" / "graphs" / p, OUT / "figures" / p)
doc = doc.replace("../../results/graphs/", "figures/")
# tables included as \input{../../results/tables/...}
for p in sorted(set(re.findall(r"\\input\{\.\./\.\./results/tables/([\w.-]+)\}", doc))):
    (OUT / "tables").mkdir(exist_ok=True)
    shutil.copy(ROOT / "results" / "tables" / p, OUT / "tables" / p)
doc = doc.replace("\\input{../../results/tables/", "\\input{tables/")

shutil.copy(PUB / "references.bib", OUT / "references.bib")
shutil.copy(SAGEJ, OUT / "sagej.cls")

def latex_escape(s):
    return s.replace("&", "\\&").replace("%", "\\%").replace("’", "'")

main = r"""% Manuscript for Social Science Computer Review, SAGE journal class (sagej.cls).
% References in APA 7 style with biblatex-apa (compile with biber; Overleaf does
% this automatically). Anonymized for review: identifying information is in
% title_page.tex.
\documentclass[Afour,times]{sagej}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[english]{babel}
\usepackage{csquotes}
\usepackage[style=apa,backend=biber]{biblatex}
\addbibresource{references.bib}
\usepackage{longtable,booktabs,array,calc}
\usepackage{graphicx}
\usepackage{float}
\usepackage[section]{placeins}
\usepackage{xcolor}
\usepackage{url}
\usepackage[colorlinks,citecolor=blue,urlcolor=blue,linkcolor=blue]{hyperref}
\usepackage{newunicodechar}
\newunicodechar{→}{\ensuremath{\rightarrow}}
\newunicodechar{−}{\ensuremath{-}}
\newunicodechar{≈}{\ensuremath{\approx}}
\newunicodechar{±}{\ensuremath{\pm}}

% Macros emitted by Pandoc
\providecommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
\makeatletter
\newsavebox\pandoc@box
\newcommand*\pandocbounded[1]{%
  \sbox\pandoc@box{#1}%
  \Gscale@div\@tempa{\textheight}{\dimexpr\ht\pandoc@box+\dp\pandoc@box\relax}%
  \Gscale@div\@tempb{\linewidth}{\wd\pandoc@box}%
  \ifdim\@tempb\p@<\@tempa\p@\let\@tempa\@tempb\fi
  \ifdim\@tempa\p@<\p@\scalebox{\@tempa}{\usebox\pandoc@box}%
  \else\usebox{\pandoc@box}\fi}
\makeatother
\providecommand{\passthrough}[1]{#1}
\def\LTcaptype{none}

\makeatletter\def\journalname{Social Science Computer Review}\makeatother

\begin{document}

\title{""" + latex_escape(title) + r""": """ + latex_escape(subtitle) + r"""}
\runninghead{Anonymized for review}
\author{Anonymized for review}
\affiliation{Anonymized for review}
\email{Anonymized for review}

\begin{abstract}
""" + latex_escape(abstract) + r"""
\end{abstract}

\keywords{""" + ", ".join(keywords) + r"""}

\maketitle
% sagej numbers footnotes with symbols, which run out after nine
\renewcommand{\thefootnote}{\arabic{footnote}}
\setcounter{footnote}{0}
""" + doc + r"""
\end{document}
"""
(OUT / "main.tex").write_text(main)

# Separate title page with the identifying information (not sent to reviewers)
subprocess.run(["quarto", "pandoc", "title_page.qmd", "-s", "-o", str(OUT / "title_page.tex"),
                "-V", "documentclass=article", "-V", "papersize=letter", "-V", "fontsize=12pt",
                "-V", "geometry=margin=1in"], cwd=PUB, check=True)
print("Wrote", OUT / "main.tex")
