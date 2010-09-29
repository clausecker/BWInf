#!/usr/bin/env python
import sys

from pygments import highlight
from pygments.lexers.functional import HaskellLexer
from pygments.formatters.latex import LatexFormatter

lx = HaskellLexer()
fmt = LatexFormatter(style='default')

preamble = fmt.get_style_defs()

outfile = sys.stdout

incode = False
code = ''
for line in sys.stdin:
    if line.strip() == '\\begin{document}':
        outfile.write('\\usepackage{fancyvrb}\n')
        outfile.write('\\usepackage{xcolor}\n')
        outfile.write(preamble)
        outfile.write(line)
    elif line.strip().startswith('\\begin{code}'):
        incode = True
    elif line.strip().startswith('\\end{code}') and incode:
        highlight(code, lx, fmt, outfile)
        incode = False
        code = ''
    elif incode:
        code += line
    else:
        outfile.write(line)
