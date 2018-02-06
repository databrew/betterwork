# Creating dictionaries for the World Bank's Better Work dashboard

_A step-by-step guide_

## Welcome

Congratulations! You are part of an elite team tasked with creating a data dictionary for the World Bank's Better Work dashboard. 

## Why are we doing this?

We are building a web application for data coming from 4 surveys done on Haitian factories. All the data for the 4 surveys are combined into one dataset. The problem is that the survey names are coded (indecipherable), and so are the responses. This dictionary provides a meachine-readable document through which we can translate survey column names (variables) and responses to plain English.

## What do we do?

- Click [here](https://docs.google.com/spreadsheets/d/e/2PACX-1vSl0V5hugIXVVS7ACXHvXu1hHlgkFbPbkkmGNKWsRZku-EKlCwfA64Y_qscLVq_j2UGtj99bc4ASE4Q/pubhtml) to open a spreadsheet with a partially filled out dictionary. This is where we'll be working.
- Click [here](https://github.com/databrew/betterwork/blob/master/Documentation/Jordan%20Surveys/pdf/all_jordan_dictionaries.pdf) to get a pdf with all the original surveys.

## How do we fill out the spreadsheet?

- Do not modify the red columns (`variable`, `stata_label`, `response`).
- Do not create additional rows or columns.
- Do not modify the `response_translation` column if it's already filled out (some already are filled out with "Yes", "No", or NA).
- Do not modify the `survey` column.
- Fill out those columns which contain `_translation` in the name.
- If there are issues (problems, missing data, etc.), write in the `comment` column.

## What do the columns mean?

There are 8 columns in the dictionary:

-`variable`: is the variable name as it appears in the original dataset.  
-`variable_translation_short`: is *OUR* short translation of the variable.  
-`variable_translation_long`: is *OUR* long translation of the variable.	  
-`stata_label`: is the variable label associated with the variable name in stata. We won't be using this at all, but it can be very helpful when searching for the corresponding questions in the survey documents.  
-`response`: is the coded response as it appears in the original dataset.  
-`response_translation`: is *OUR* translation of the coded response.  
-`survey`: is which of the 4 surveys this question is from.  
-`comment`: is reserved for writing about issues, problems, etc.  

## Okay, what's the process?

### Translating the variables 

- You've now opened the spreadsheet and the pdf of the original survey (if not, go back up [here](https://github.com/databrew/betterwork/blob/master/dictionaries/creating_dictionaries_jordan.md#what-do-we-do)). 
- Go through line-by-line. Find the `variable` in question in the original survey documentation. Based on the survey question, create both `variable_translation_short` and `variable_translation_long` entries (use your judgement). These should be meaningful and correct grammatically (capitalizations, symbols, etc. allowed).

### Translating the responses

- Also fill out the `response_translation` field. 
- There is one row for each variable-response pairing. So, for any given variable, the `variable_translation_short` and `variable_translation_long` fields will be identical, but the `response_translation` fields will not be. This means you'll use a lot of copy+paste for filling out `variable_translation_short` and `variable_translation_long`, but not for `response_translation`.
- If the `response` is `"<NUMERIC>"`, put nothing in the `response_translation` field.
- If the `response` field for a variable is not `"<NUMERIC>"`, but you think it should be, then delete all but one row for that variable, and replace the `response` field with `"<NUMERIC>"`

## A few additional comments

- If workings simultaneously with someone else, it's advisable to work in different sections of the spreadsheet.
- If you have any issues, put them in the `comment` field.
- If you have major issues, write Joe.
- Be creative in your searching. The variable names in the data don't align closely with those in the pdf (sometimes they're completely different), so you may have to make some inference based on order, which survey, similarity of names, and the `stata_label`, etc.
- If you're not sure of something, but it's your "best guess" (based on order, intuition, etc.), then fill it in and place something in the `comment` field.
- The dataset includes data from 4 surveys. The variables go in the order of the pdf. If you are working on just one survey, and don't want to be searching the entire pdf, consider using the survey-specific documents [here](https://github.com/databrew/betterwork/tree/master/documentation/Jordan%20Surveys) - these are the original survey documents.
