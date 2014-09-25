=======================================================================
Optional: Explain any difficulties you had with this assignment or any
parts that are incomplete.  Suggestions on how this assignment could
be improved to help students learn more are welcome:

I finished everything, but I feel like there is a better way to
write transpose and perm stylistically.  In the future, it would have
been helpful to name warmup something that better suggests the relative 
lengths of the parts of the assignments.  The warmup took ~3x as long as
the query part of the assignment.


=======================================================================
For each of the following questions, 

(A) give the answer 

(B) give the shell command (or commands) you used to answer the
question and in a sentence, explain why your shell command gives you
helpful data.  (FYI: using fewer commands and piping is more elegant
than using more commands and will be given more credit).

Use the data given in the files G.txt, PG.txt, PG-13.txt, R.txt for
the set of top-grossing films in each rating category (G, PG, PG-13
and R).  Use the data in the file alltime.txt for the alltime
inflation-adjusted returns.

1.  Adjusting for inflation, what is the top-grossing film of all time
(use the alltime.txt data)?

(A) Gone with the Wind

(B) ./boxoffice -sort-gross < data/alltime.txt | ./boxoffice -take 1
	This command first sorts all of the films in alltime.txt by gross
	and then only takes the first one.

2. What is the 50th ranked R film by gross on the list?

(A) The Silence of the Lambs

(B) ./boxoffice -sort-gross < data/R.txt | ./boxoffice -take 50 | ./boxoffice -drop 49
	This command once again sorts by gross, then only prints the 50th
	one because it takes the first 50 on the list and then drops the first
	49 of them.

3. Suppose you had a chance to make 1 film with a top director and the
director was so good you were guaranteed that whatever film you made
would be in the top 5 grossing films in its ratings category (G, PG,
PG-13, R) --- and equally likely to be ranked 1, 2, 3, 4, or 5.  What
rating (G, PG, PG-13, R) would you choose to give your film if you
wanted to make the most money?

(A) PG-13

(B) ./boxoffice -sort-gross < data/G.txt | ./boxoffice -take 3 | ./boxoffice -drop 2 & \
	./boxoffice -sort-gross < data/PG.txt | ./boxoffice -take 3 | ./boxoffice -drop 2 & \
	./boxoffice -sort-gross < data/PG-13.txt | ./boxoffice -take 3 | ./boxoffice -drop 2 & \
	./boxoffice -sort-gross < data/R.txt | ./boxoffice -take 3 | ./boxoffice -drop 2

	With uniform distribution across rankings, I want to maximize 
	the middle value, rank 3.  To do this, my command prints the third 
	place movie in every rating category to show that The Dark Knight is the
	highest grossing 3rd movie in the rating category of PG-13.  This is
	approaching the command size where a separate script would be useful, 
	but for this example this command works fine.

4. Taking inflation in to account, would you have preferred to make
money off of blockbusters in the 70s or in the 80s?

(A) The 70s.

(B) ./boxoffice -decade 70 <data/alltime.txt | ./boxoffice -average  Then, 
	./boxoffice -decade 80 <data/alltime.txt | ./boxoffice -average

	Both commands show the average gross of all of the movies in that
	decade, adjusted for inflation.  The 70s averaged $499m while the
	80s averaged $421m. 

5. Taking inflation in to account, which studio made the most
money off of blockbusters in the 60s?

(A) AVCO

(B) ./boxoffice -decade 60 <data/alltime.txt |./boxoffice -by-studio |./boxoffice -sort-studio
	This command prints, the total revenues of all studios in the 60s, in order of greatest to 
	least.  The top of this list was AVCO at $670m.

