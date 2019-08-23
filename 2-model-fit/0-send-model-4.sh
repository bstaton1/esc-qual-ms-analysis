#PBS -q fastfat -l nodes=node144:ppn=5,walltime=60:00:00,flags=ADVRES:liuzhan_ff
#PBS -d .

###email me
#PBS -M bas0041@auburn.edu
#PBS -m abe

###files to write out
#PBS -o progress-4.txt
#PBS -e errors-4.txt

###make JAGS module findable
export LD_LIBRARY_PATH=/tools/jags-4.3.0/lib64:$LD_LIBRARY_PATH

###load your modules once on the node
module load R/3.4.3
module load jags/4.3.0
module load blas/gcc/3.6.0

###send the job
echo "Running Model"
Rscript 2-fit-model-any.R 4
echo "Model Fitting Done"
