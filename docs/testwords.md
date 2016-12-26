# Words to Test Tali Forth with

Scot W. Stevenson <scot.stevenson@gmail.com>
First version 15. March 2014
This version 26. Dec 2016

## General words

```
: lat ." <foo>" ;
: flat ." <bar>" >in dup @ 3 - swap ! ; \ flat prints "<bar><foo>"

char & parse jack& type                 \ should print "jack"    
```


## Loop testing

Test normal loop:
```
: aaa 11 1 do i . loop ;        
```
Should produce numbers 1 - 10

Then, try this as multi-line loop because that can be tricky:
```
: bbb 11 1 do 
  i . loop ; 
```

Put IF in the loop:
```
: ccc 11 1 do  i 5 > if i . then loop ; 
```
should produce "6 7 8 9 10"

TEST nested loops (see https://www.forth.com/starting-forth/6-forth-do-loops/):
```
: ddd cr 11 1 do
    11 1 do 
        i j * 5 u.r 
    loop cr loop ;
```
should produce math table from 1x1 to 10x10

Test UNLOOP and EXIT:
```
: eee 11 1 do i dup 8 = if drop unloop exit then . loop ." Done" ; 
```
should produce "1 2 3 4 5 6 7"

Test LEAVE
```
: fff 11 1 do i dup 8 = if leave then . loop ." Done" drop ; 
```
should produce "1 2 3 4 5 6 7 Done"

The Data Stack should be empty after all of these words, check with .S

