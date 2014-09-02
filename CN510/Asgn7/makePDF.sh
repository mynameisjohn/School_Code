let nargs=$#
if [$nargs == 0]; then
echo "Input file is missing. Please type the name of your .tex file (no .tex)"
else
latex $1.tex
dvips $1.dvi -Ppdf -G0 -o $1.ps
ps2pdf $1.ps 
echo "Done"
fi