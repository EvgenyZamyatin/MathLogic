cd tests/
for d in */; 
do 
cd $d
>test_list.txt
for f in *.in 
do
    echo "$f" >> test_list.txt 
done
cd ../
done 

