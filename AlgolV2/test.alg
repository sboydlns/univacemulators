begin comment this is the main program starting point;
   integer array a, b[0:9, 1:10];
   integer i, j;
   real2 f, f1;
   boolean done;
   local label l1, l2, l3;
   switch s1 = l1, l2, l3;
   string str1[10, sub1[sub2[5], sub3[15]]], str2[5];
   string array str3[6:1:10];

   i := f;
   
   str1 := 'abcdefg';
   
   write(console, 'i = ', i, 'j = ', j);
   if str1 lss str2 then goto l1;
   if str1 leq str2 then goto l1;
   if str1 gtr str2 then goto l1;
   if str1 geq str2 then goto l1;
   
   for i := 1 while not done do done := true;
   
   for i := 1 step 1 until i geq 10 do i := i * 2;
   
   for i:= 1, 2, 3, 4 do i := i * 2;
   
   goto s1[i];
      
l1:
   if i eql 1 then
      goto l1 
   else 
      goto l2;
   
   goto if i eql 1 then l1 else if i eql 2 then l2 else l3;   
l2:
l3:
   
   a[0, 1] := 1;
   
   i := a[0, 1] + i;
   
   f := -f;
   
   i := -1;
   
   f := f + if f eql 1 then 2.0 else 3.0;
   
   i := if i eql 1 then 2 else if i eql 2 then 3 else 4;
   
   f := f + i / 2;

   i := (i + j) // 100000;
   
end of main program;