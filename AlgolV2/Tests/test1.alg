begin

comment Calculate the first 20 Finonacci numbers using a variety
        of different looping methods;
        
integer f1, f2, f3, count;
integer array f[1:20];

   write(console, 'fibonacci numbers');
   
   f[1] := 0;
   f[2] := 1;
   f1 := 0;
   f2 := 1;
   count := 3;
loop:
   f3 := f1 + f2;
   f[count] := f3;
   f1 := f2;
   f2 := f3;
   count := count + 1;
   if count leq 10 then
      goto loop; 
  
   for count := count step 1 until 20 do
   begin
      f3 := f1 + f2;
      f[count] := f3;
      f1 := f2;
      f2 := f3;
   end;
   
   count := -4;
   for count := count + 5 while count leq 20 do
      write(console, f[count], f[count + 1], f[count + 2], f[count + 3], f[count + 4]);
   
end;   