# Molecular characterisation of human rabies in Tanzania and Kenya: a case series report and phylogenetic investigation
Repository for: Jaswant *et al*. *Infectious Diseases of Poverty* 2024 [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13934433.svg)](https://doi.org/10.5281/zenodo.13934433)
Gurdeep Jaswant*, Kathryn Campbell*, Anna Czupryna, Athman Mwatondo, Brian Ogoti, Carmen W.E. Embregts, Corine H. GeurtsvanKessel, Charles Kayuki, Davis Kuchaka, Gati Wambura, James Oigo, Joel Changalucha, Julius O. Oyugi, Kennedy Lushasi, Lwitiko Sikana, Marco van Zwetselaar, Marieke C.J. Dekker, Mathew Muturi, Marybeth Maritim, Mumbua Mutunga, Rowan Durrant, Tom Abala, Veronicah Chuchu, Kirstyn Brunker*, S.M. Thumbi* & Katie Hampson*

*Equal contributions 

This repository contains all the code and de-identified data in this study.

Analyses were undertaken using R version 4.3.0.
Code and analyses take minutes to run. 
Additional details on genetic data and alignments etc undertaken outside of R are detailed in the methods.

## About
Rabies remains a major public health problem in East Africa where the disease is rarely laboratory-confirmed. We use virus sequence data to enhance investigations of five human cases, revealing important implications at individual, healthcare, and societal levels. The cases differed in their epidemiological context and care: three of the bite victims did not receive post-exposure vaccination; one received only the first dose while the last was vaccinated following a timeline not recommended by WHO. These discrepancies raise concerns about health-seeking behaviour, competency of healthcare professionals in handling rabies-exposures and accessibility and effectiveness of post-exposure prophylaxis as administered in these settings. Our investigation confirms dog-mediated rabies as the cause of these deaths, highlighting the regional circulation of rabies within domestic dog populations. We conclude that urgent action is needed to improve awareness of rabies post-exposure prophylaxis, ensure its accessibility and appropriate administration, and for coordinated dog vaccination to control dog-mediated rabies.

## Methods
This paper combines epidemiological and genomic analyses briefly described below:

1. Amplicon-based sequencing was conducted following a previously established [protocol](https://app.jove.com/b/65414/a-cost-effective-genomic-workflow-for-advancing-rabies-control)

2. Bioinformatics - [A RABV-tailored version](https://github.com/kirstyn/artic-rabv) of the ARTIC network’s bioinformatics pipeline was applied to process raw MinION reads 

3. Phylogenetic analysis

a) [RABV-GLUE](http://rabv-glue.cvr.gla.ac.uk/#/rabvFastaAnalysis) was used for major and minor clade assignment 

b) More resolved lineage designation was done using [MADDOG](https://github.com/KathrynCampbell/MADDOG.git), sequenced viruses were classified using the nomenclature <Major clade Minor clade_Lineage>, e.g., Cosmopolitan AF1a_A1.1. The MAD DOG tool is publically available as a command line tool and an R package and on github 

c) Publicly available sequences from identified lineages were obtained from RABV-GLUE and aligned with newly generated sequences using the MAFFT FFT-NS-2 algorithm.
```bash
mafft  /path/to/consensus/*.fasta > aligned_sequences.fasta
```

d) Maximum likelihood trees were built using IQTREE2 with model selection and 1000 ultrafast bootstrap replicates.
```bash
iqtree -s aligned_sequences.fasta -bb 1000
```
e) Patristic distances were calculated using the get_pairwise_distances function from the R package castor. Trees of widespread lineages were checked to identify potential incursions relevant to human cases and divergence times between detected cases: [distance_to_time.R](https://github.com/Gurdeepjaswant/EA_human_rabies_case_series/blob/main/script/distance_to_time.R) 

f) Locations of cases were mapped using village, district or county centroids using:
[Figure2.R](https://github.com/Gurdeepjaswant/EA_human_rabies_case_series/blob/main/script/Figure2.R) and
[Figure3.R](https://github.com/Gurdeepjaswant/EA_human_rabies_case_series/blob/main/script/Figure3.R)
