""" M3C 2016 Homework 1
Python script which prints name and college id
To run this code, enter "python hw1.py" at the terminal
"""

#1. modify the list, Output, so that it contains your name and college id
Output = ["Mathilde Duverger","00978498"]

#2. modify x and y in the print statements below so that your name and college id are output
print "M3C 2015 Homework 1 by", Output[0]
print "CID:", Output[1]


#3. Add python code here which removes any leading zeros from your CID and stores the result in
#   the variable, ID2. For example, "00000001" would become "1"
ID2 = Output[1].strip('0')
print "ID2:", ID2

#4. Add python code here which removes all zeros from your CID and stores the result in
#   the string, ID3. For example, "10000001" would become "11"
ID3 = Output[1].replace('0','')
print "ID3:", ID3


#Note: Your code for parts 3 and 4 should work for any 8-digit CID with at least one non-zero
#number. 
