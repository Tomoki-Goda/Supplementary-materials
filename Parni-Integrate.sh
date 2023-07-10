#!/bin/bash

#Color for printing
color="\e[38;5;200m"
back="\e[m"
mathdir="/home/tomoki/beam-functions/n3lo-TG/"



printf "${color}This file will do all the work-flow, occasionally ask you to choose options! ${back}\n\n \a " 

printf "${color}Current defaut diectory is ${mathdir}. \n Where is the directory \" integral\" ? \n (relative to the default directry. start without / and end in /) : ${back}\n"
read logdir


intdir="${mathdir}${logdir}integrals/"

printf "${color}integral directory is ${intdir} ${back}\n\n " 

if [ -d ${mathdir}${logdir}runs ] ; then 
echo "nothing to remove"
else
mkdir ${mathdir}${logdir}runs 
fi



#intdir="${mathdir}beamcalc/integrals/"
runsdir="${mathdir}${logdir}runs/"

echo "${color}${runsdir}"
echo "${color}${intdir}"

#clean old files
for i in `find ${runsdir} -name int*.dat`
do
rm $i
done

echo "${runsdir} cleaned"



for i in `find ${intdir} -name int*.dat`
do 
echo $i
done

printf "\n\n${color}List out the sector numbers you want to integrate${back}. Or \"All\" for all.\n \a"
read sectors
if [ "${sectors}" == "All" ];then
	echo "How many sectors?"
	read maxnum
	sectors=1
	for ((i=2;i<=${maxnum};i++));do 
	sectors="${sectors} ${i}"
	done
	echo "${sectors}"
fi


parnidir="${mathdir}Parni/"

for i in `find ${mathdir}${logdir} -name *.o `
do
rm $i
done

for i in ${sectors}
do 
#echo $i
${parnidir}prepare.sh $i ${mathdir}${logdir}
cp  ${mathdir}${logdir}fillmap.hh  ${parnidir}
cp  ${mathdir}${logdir}includefiles.hh  ${parnidir}
make --dir ${parnidir} NAME=$i LOGDIR=${mathdir}${logdir} --silent

done

rm ${runsdir}list.dat
#clean old files


for i in ${sectors}
do 
	for j in `ls ${runsdir}int${i}*.dat`
	do
		echo "${mathdir}${logdir}integral${i} ${j}">> ${runsdir}list.dat
	done
done

cat ${runsdir}list.dat | parallel 


grep -h int  ${mathdir}${logdir}resint*>${mathdir}${logdir}repl
