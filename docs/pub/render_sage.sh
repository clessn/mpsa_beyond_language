#!/usr/bin/env bash
# Renders the three files Social Science Computer Review (SAGE) asks for:
#   pub.pdf             full manuscript (authors shown)
#   pub_anonymized.pdf  manuscript for double-anonymized review (mask: true)
#   title_page.pdf      separate title page with identifying information
#   supplement.pdf      supplemental tables S1-S5 (anonymized)
set -euo pipefail
cd "$(dirname "$0")"
# The masked render goes first: "--output" deletes the default pub.pdf
quarto render pub.qmd -M mask:true --output pub_anonymized.pdf
quarto render pub.qmd
quarto render title_page.qmd
quarto render supplement.qmd
