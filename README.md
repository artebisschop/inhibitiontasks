# inhibitiontasks
Master Thesis 2021
Arte Bisschop

### 01 Creating .db file ###
With the code in this file, the database was created. First, all seperate datasets get loaded and formatted, 
then, the mean and sd effects get calculated and then, the "dataset", "study" and "task" table are created.
Finally, the .db file is created using SQLite.

### 02 BF analyses ###
To run the code in this file, you will need the inhibitiontasks.db file.
The code in 02 BF analyses uses the "quid" package to calculate the Bayes factors that reflect the relative
plausibility of each of the three models. The results are saved in "results.rda" (but because this file is 
1.79 GB, it could not be uploaded in the repository).

### 03 Creating Shiny input ###
Because of computational reasons, a modified table with the dataset properties is created in this file. 
The Shiny app calls on this table and on the .png plot files that are also created with the code and the
result.rda file.

### shiny ###
This folder contains the code used to create the Shiny app (see https://artebisschop.shinyapps.io/shiny/)
