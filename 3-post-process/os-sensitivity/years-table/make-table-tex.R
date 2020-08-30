# some packages
library(knitr)
library(kableExtra)

# create preamble
preamble = c(
  "\\documentclass[12pt]{article}", 
  "\\usepackage[paperheight=4in,paperwidth=6in,margin=0.1in]{geometry}", 
  "\\usepackage{mathpazo}",
  # "\\usepackage{sansmathfonts}",
  # "\\usepackage{helvet}",
  # "\\renewcommand{\\rmdefault}{\\sfdefault}",
  "\\usepackage{amsmath}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{array}",
  "\\usepackage{multirow}",
  "\\usepackage[table]{xcolor}",
  "\\usepackage{wrapfig}",
  "\\usepackage{float}",
  "\\usepackage{colortbl}",
  "\\usepackage{pdflscape}",
  "\\usepackage{tabu}",
  "\\usepackage{threeparttable}",
  "\\usepackage{threeparttablex}",
  "\\usepackage[normalem]{ulem}",
  "\\usepackage{makecell}",
  "\\begin{document}", 
  "\\pagenumbering{gobble}"
)



# read in the raw input table
tab = read.csv("years.csv", stringsAsFactors = F, allowEscapes = T)

# replace "\\n" with "\n": for kableExtra::linebreak
tab = apply(tab, 2, function(x) stringr::str_replace(x, "\\\\n", "\\\n"))

# add line breaks to necessary cells
tab = apply(tab, 2, function(x) linebreak(x, align = "c"))

# set the color for cells not considered by model
tab[tab == "color"] = cell_spec(" ", format = "latex", background = "#D3D3D3")

# pretty latex column names
colnames(tab) = c("\\textbf{Year}", "$\\boldsymbol{y}$","$\\boldsymbol{t}$", "\\textbf{Age-4}\n($a=1$)",	"\\textbf{Age-5}\n($a=2$)",	"\\textbf{Age-6}\n($a=3$)",	"\\textbf{Age-7}\n($a=4$)")
colnames(tab) = linebreak(colnames(tab), align = "c")

# create caption
cap = "Structure of the brood and calendar year indexing in the state-space model.
  The brood year $y$ in which fish returning in calendar year $t$ at age $a$ is calculated as $y = t + n_a - a$.
  Age and year indexing were identical for both males and females.
  Grey regions are years that are not included in the calculations. 
  Note that in order to have model-predicted age-structured return in $t = 1$ for fitting to data observed that year, 
  $a_{\\text{max}}$ brood years before $t = 1$ must be included (larger grey region). 
  Similarly, even though escapement data span until 2019, the model does not track recruitment states after 2015
  because no adults had yet been observed from later brood years (smaller grey region).
  "

# create table code
table_code = kable(tab, "latex", booktabs = T, longtable = F,
          linesep = "", escape = F, align = "ccccccc") %>%
  kable_styling(full_width = F) %>%
  add_header_above(c("Year Indices" = 3, "Brood Year of Returning Adults in $\\\\boldsymbol{t}$" = 4), bold = T, escape = F) %>%
  as.character()

tex = paste(
  c(preamble,
    table_code,
    "\\end{document}"
  ),
  collapse = "\n"
)

writeLines(tex, "table.tex")

tinytex::latexmk("table.tex", "pdflatex", clean = T)
file.show("table.pdf")
unlink("table.tex")
