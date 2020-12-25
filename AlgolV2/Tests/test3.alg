begin

   comment Test unformatted WRITE command;

   real f1;
   integer i, j, count, exp;
   real array a[1:10, 1:5];
   string s1[s1h1[5],s1h2[5]], s2[10];
   
   count := 1;
   for i := 1 step 1 until 5 do
   begin
      for j := 1 step 1 until 10 do
      begin
         a[j, i] := count;
         count := count + 1;
      end;
   end;
   write(console, a);
   
   s1h1 := 'h1';
   s1h2 := 'h2';
   s2 := 'testing';
   write(console, s1, s1h1, s1h2, s2);
   
   write(console, true, false);
   write(console, false, true);
   
   f1 := -123;
   write(console, f1);
   write(console, 1.2345);
   write(console, -123.456);
   write(console, .0123456);
   write(console, 0.123456);
   f1 := 10.0;
   for i := 1 step 1 until 6 do
   begin
      exp := f1 ** i;
      write(console, exp);
   end;      

end