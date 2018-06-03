# biogeography
The project about geography influence on genome of plant.

## The goal
1. Determine the climate and soil influence on genome of
modeling plants.
2. Build a model base on genome and environment data.

## Tasks
1. Get familiar with WorldClim and Soil databases
2. Download 1001 genome SNPs data and collection coordinates of arabidopsis
3. Get climate and soil data associated with this points
4. Convert SNPs to vectors by Admixture tools
5. Design model
6. Interpretation of result

## Methods used
1. Admixture component analyses
2. Statistical classifycation methods

## Packages and dependencies
1. Tools
..* admixture
..* PLINK
..* vcftools
2. R packages
`install.packages(c(
    "psych", 
    "raster",
    "sp",
    "rgdal",
    "RSQLite",
    "corrplot",
    "caret",
    "rpart",
    "rpart.plot",
    "randomForest",
    "Cairo",
    "rworldmap",
    "rworldxtra",
    "ade4"))`
3. Python3 packages
`
import(pandas)
import(numpy)
import(matplotlib)
import(seaborn)
import(sklearn)
import(xgboost)
`

## Example
See example in ./corr_pca_rndf

## Databases
1. Climate raster database http://www.worldclim.org/
2. Soil database http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/
3. 1001 genom project http://1001genomes.org/data/GMI-MPI/releases/v3.1/
