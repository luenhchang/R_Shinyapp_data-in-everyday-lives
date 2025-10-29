## To push selective files and folders from [your local directory] (C:\GoogleDrive_MyDrive\scripts\RProject_Shinyapp_data-in-everyday-lives) to a GitHub repository, follow these steps

## Initialize a Git Repository (if not already done)
Open a terminal (or Git Bash if you're on Windows).

Navigate to the directory:
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
```

Initialize the Git repository:
```bash
git init
```
This creates C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/.git

## Add a Remote Repository
Go to your GitHub account and create a new repository 

Copy the repository's URL https://github.com/luenhchang/R_Shinyapp_data-in-everyday-lives.git

Add the remote to your local Git repository:
```
bash
git remote add origin https://github.com/luenhchang/R_Shinyapp_data-in-everyday-lives.git
```

## Selectively Stage Files and Folders
Stage (select) files and folders:
```bash
git add global.R server.R ui.R menuItem-About.html menuItem-About.Rmd webapp-printscreens/
```

Verify the staged files. This will list new files to commit and untracked files (files, folders that won't be committed)
```bash
git status
```

## Commit the Changes
Commit the staged files with a meaningful message
```bash
git commit -m "Add application files and folder to remote"
```

## Push to GitHub
Push the committed changes to the main branch of your GitHub repository:
```bash
git push origin main
```

## Create a .gitignore file
If you want to use git add . to stage all changes in the directory but still exclude certain files and folders from being committed to the repository, creating a .gitignore file is the best approach

Navigate to your project directory:
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
```

Create a .gitignore file:
```bash
touch .gitignore
```
Or, create it using a text editor (e.g., Notepad, VS Code).

## Define Files/Folders to Ignore
Add the files and directories you want to exclude to the .gitignore file. 
```gitignore
# Ignore specific files
backup.txt
complex-heatmap.R
Heatmap-in-R-Static-and-Interactive-Visualization.R
push_selective_files_folders_from_local_to_Github_repository.md
RFunction_Run-Seurat-singleCellRNAsequencing-CITEsequencing.R
RFunction_Run-Seurat-singleCellRNAsequencing-CITEsequencing_normalise-scale.R
RFunction_train-cell-classifier.R
sc_RNASeq_CD8.R
sc_RNASeq_CD8.sh

# Ignore specific folders
Garnett-pre-trained-classifiers-for-cell-types/
references/

# Ignore log files
*.log

# Ignore temporary files
*.tmp
*.bak
```

## Stage and Commit the .gitignore File
Once the .gitignore file is set up, commit it to the repository so that Git knows to respect it
```bash
git add .gitignore
git commit -m "Add .gitignore file to exclude unwanted files and folders"
```

## Using git add . Safely for subsequent changes
After setting up the .gitignore file:

1. Use git add . to stage all files except those listed in .gitignore:\
```bash
git add .
```

2. Verify the staged files with:
```bash
git status
```

3.Commit and push changes:
```bash
git commit -m "Commit all changes"
git push origin main
```

## Create README.md
### Option 1: Create README.md Locally
Advantages:
* Offline Editing: You can write and format the README.md without needing an internet connection.
* Familiar Tools: Use your favorite text editor (e.g., VS Code, Sublime Text, or Notepad++) to write the file.
* Version Control: You can easily track changes to the README.md file in your local repository and commit those changes as part of your version history.

Steps:
Create the file locally:
```bash
touch README.md
```

Edit the file using your preferred editor.

Add, commit, and push the file to GitHub:
```bash
git add README.md
git commit -m "Add README.md"
git push origin main
```

## 2025-01-13 Changes upload
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added functions.R, updated global.R, server.R, ui.R, README.md"
git push origin main
```

## 2025-01-24
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added number of container collected or refunded in 2025 stacked bar plot using plotly"
git push origin main
```

## 2025-01-26
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added Recycling 2025, 2024 valueBoxes"
git push origin main
```

## 2025-02-03
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Corrected placement of text label on stacked bars of daily number of collected or refunded containers by plotly"
git push origin main
```

## 2025-02-07
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added horizontal bar plots for employment events and job application events"
git push origin main
```

## 2025-08-18
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added electricity usage and solar export chart, table and rate table"
git push origin main
```

## 2025-08-20
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added electricity consumption amount paid value box"
git push origin main
```

## 2025-08-21
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Added current rates and charges valueBox"
git push origin main
```

## 2025-09-21
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Updated parse_LLMWhisperer_PDF.R to extract 2025 Aug tables"
git push origin main
```

## 2025-09-28
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Updated parse_LLMWhisperer_PDF.R to extract Your meter readings table"
git push origin main
```

## 2025-10-01
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Updated parse_LLMWhisperer_PDF.R to read Alinta-energy-bill_supply-period_20250901-20250915.txt"
git push origin main
```

## 2025-10-29
```bash
cd "C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives"
git add .
git commit -m "Created export-gsheet-data.R"
git push origin main

