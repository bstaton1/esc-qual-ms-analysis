#NODE-SETTINGS-HERE
#PBS -d .

###email me
#PBS -M bstaton@critfc.org
#PBS -m abe

###files to write out
#PBS -o progress-MODEL.txt
#PBS -e errors-MODEL.txt

###make JAGS module findable
export LD_LIBRARY_PATH=/tools/jags-4.3.0/lib64:$LD_LIBRARY_PATH

###load your modules once on the node
module load R/3.4.3
module load jags/4.3.0
module load blas/gcc/3.6.0

###send the job
echo "Running Model: MODEL"
Rscript 2-fit-model-any.R MODEL
echo "Model Fitting Done"
