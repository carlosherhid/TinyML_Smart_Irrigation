#! /bin/bash
wd=/home/aurora/RiegoInteligente/code/
nohup Rscript ${wd}run.R -r > ${wd}logOUT.txt 2> ${wd}logERR.txt < /dev/null &
