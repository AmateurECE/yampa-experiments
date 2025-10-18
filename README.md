# yampa-experiments

# Building the Diagrams

To build, e.g. the Stage 1 Design diagram:

```
lualatex Stage-1-Design.tex
magick -density 300 Stage-1-Design.pdf -quality 90 Stage-1-Design.svg
```
