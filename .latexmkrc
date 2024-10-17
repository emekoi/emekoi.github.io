@default_files = ('resume/resume.tex');
ensure_path('TEXINPUTS', './resume/');
$pdf_mode = 1;
$pdflatex=q/pdflatex %O -shell-escape -halt-on-error %S/;
$pdf_update_method = 0;
$pdf_previewer = 'zathura';
