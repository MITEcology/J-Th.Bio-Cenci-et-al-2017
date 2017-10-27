#!/bin/sh
Rscript Main.r
sleep 20
python MeanShift.py
sleep 10
Rscript PltFunction.r
evince Rplots.pdf
