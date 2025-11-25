


<img  width="300" src = "www/SCAN_logo1.png"><h1 style = "float:letf;"> SCAN_V1 </h1>
a quantitative framework to identify and map Biogeographical Units (Chorotypes) based on species spatial congruence


### What is SCAN about?
In Biogeography, the field in biology that studies the spatial distribution of species and the evolution of their environments, there is a pervasive question: - Why unrelated species often have similar geographic distributions?. The phenomenon of congruence in geographical distributions lead Wallace, the father of Biogeograpy and co-founder of the the Theory of Evolution by Natural Selection, for example, but also many other researchers, to insighst about evolution and ecology of species and environments. Congruence, ideally, should be among the main criteria for any analysis in biogeography based in species' distributions. However, following a strict distributional approach, until now, most methods did not use the spatial congruence between these distributions as an explicit (and controlable) parameter.

**How about obtaining direct measures of spatial congruence between species distributions, pairwise, and using them as criteria for connection in a network of spatial relationships?** SCAN explores species-to-species spatial relationships in an enviroment of a single-layered network analysis. The most obvious spatial patterns of shared distributions are evident when shallow network relationships are found. However, when higher depths in the network are explored a plethora of other spatial relationships can be recognized, including gradients of distribution, that cannot be approapriatelly described by conventional methods and, because of that, are often ignored in the field of biogeography.
<img height = 60% src = "Psophia.jpg">
### Why is it important?

The algorithm applies objective criteria regarding the spatial properties of entities, such as raw species' distributions, but can also be extrapolated to the analysis of environments and geographic regions. Recognized patterns may vary from highly congruent, when species have very similar distributions, to patterns where congruent requirement settings are relaxed and more species included. In the later case, continuous gradients can be highlighted by species overlapping or replacing each other along a transitional zone. This flexibility allows the recognition of dynamics and, perhaps, evolution in spatial patterns. More important, it allows the comparison between species and regions based on natural and objective criteria, such as number of congruent species, their level of congruence, the ratio between the shared and total areas of distribution, and many more derived from network analytics.

## 🚀 Installation & Usage

Unlike previous versions, **SCAN v0.22 runs on modern R versions** (tested on R 4.5.2). You do not need to downgrade your R installation.

### Step 1: Install R and RStudio
If you haven't already, please download and install the latest versions:
1.  **Download R:** [https://cran.r-project.org/](https://cran.r-project.org/)
2.  **Download RStudio:** [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
### Step 2: Install Required Packages
Open RStudio, copy the code below, paste it into the Console, and press Enter. This will install all necessary dependencies (including the spatial engines).

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "sf",
  "leaflet",
  "ggplot2",
  "dplyr",
  "igraph",
  "tidygraph",
  "ggraph",
  "readr",
  "units",
  "htmltools",
  "lwgeom", 
  "RColorBrewer"
))
```
Step 3: Run SCAN
Once the packages are installed, you can launch the SCAN Engine directly from GitHub by typing this in your R console:
```r
library(shiny)
# Run SCAN directly from the repository
runGitHub("SCAN_R_4.5.2", "cassianogatto")
```

### Example
The folder <strong>example</strong> brings a small sample of New World Primate distributions to practice and build your first Chorotypes!

### Perspectives...
We are applying the method to the analysis of endemic patterns of South American Birds and Primates (with collaborators). SCAN is super intuitive, allows the gattering of tons of insights about species distributions, and now is fully converted to a standard network analysis (in R). Many network tools and concepts can now be integrated to biogeographical analysis.

The **paper** with concepts is [here](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0245818) 

My **thesis** is also available from [here](https://repositorio.inpa.gov.br/bitstream/1/39803/3/tese_cassiano.pdf) (Intro in Portuguese, Chaps 1 & 2 in English).
