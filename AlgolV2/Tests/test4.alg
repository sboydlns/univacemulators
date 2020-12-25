begin
   
   comment Test string array and substring processing;
   
   string s1[w1[3], 1, w2[2], 1, w3[3], 1, w4[4], 65];
   string c[1], words[wc1[1], 19 : 1:20];
   integer i, j, k;
      
   write(console, 'initializing');
   s1 := 'NOW IS THE TIME FOR ALL GOOD MEN TO COME TO THE AID OF THEIR PARTY.';
   for k := 1 step 1 until 20 do
      words[k] := ' ';
   j := 1;
   k := 1;
   write(console, 'intialization done');

   comment Break a string into words;
   
   for i := 1 step 1 until 80 do
   begin
      c := s1[i];
      if (c neq ' ') then
      begin
         if (k leq 20) then
         begin
            if (j leq 20) then
               words[j : k] := c;
            j := j + 1;
         end;
      end else
      begin
         if (words[k] neq ' ') then
            k := k + 1;
         j := 1;
      end;
   end;
   
   comment Write the first character of each word and the
           word itself;
           
   for k := 1 step 1 until 20 do
   begin
      if (words[k] neq ' ') then
         write(console, wc1[k], words[k]);
   end;
      
   comment Disply first 4 words using substrings;
   
   write(console, ' ');
   write(console, w1, w2, w3, w4);

end;