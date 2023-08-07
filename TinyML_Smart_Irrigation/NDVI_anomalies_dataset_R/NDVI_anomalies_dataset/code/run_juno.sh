#! /bin/bash
wd=/home/gardens/RiegoInteligente/code/
nohup Rscript ${wd}run.R -v > ${wd}logOUT.txt 2> ${wd}logERR.txt < /dev/null &
