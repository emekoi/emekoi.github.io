@default_files = ('resume/resume.tex');
ensure_path('TEXINPUTS', './resume/');
$aux_dir = '_cache/tmp/';
$out_dir = '_site/static/';
$pdf_mode = 1;
$pdflatex=q/pdflatex %O -shell-escape -halt-on-error %S/;
$pdf_update_method = 0;
$pdf_previewer = 'zathura';
