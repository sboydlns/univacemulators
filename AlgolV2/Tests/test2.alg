begin

   comment Calculate the first 20 primes;

   integer count, num, test, limit;
   integer array p[1:20];
   
   write(console, 'prime numbers');
   
   num := 1;
   count := 1;
   for num := num + 2 while count leq 20 do
   begin
      if num lss 9 then
      begin
         p[count] := num;
         count := count + 1;
      end else
      begin
         local label notprime;
         limit := num // 2;
         for test := 3 step 2 until limit do
         begin
            if ((num // test) * test) eql num then
               goto notprime;
         end;
         p[count] := num;
         count := count + 1;   
notprime:         
      end;
   end;
   
   for count := 1, 6, 11, 16 do
      write(console, p[count], p[count + 1], p[count + 2], p[count +3], p[count + 4]);
       
end;
   