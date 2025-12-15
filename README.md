---
title: SCAN Analysis App
emoji: 🦜
colorFrom: green
colorTo: blue
sdk: docker
pinned: false
app_port: 7860
---

<div>
  <img src="www/SCAN_logo1.png" width="300" alt="SCAN Logo">
   </div>

```r 
SCAN -> Mapping Biogeographical Units (Chorotypes) based on a Network Analysis of Spatial Congruences 
```
### What is SCAN about?
In **Biogeography**, the field of biology that studies the spatial distribution of species and the evolution of their environments, a pervasive question remains: why do unrelated species often exhibit similar geographic distributions? The phenomenon of distributional congruence inspired Wallace — the father of Biogeography and co-founder of the Theory of Evolution by Natural Selection — as well as many subsequent researchers, to develop insights about the evolution and ecology of species and environments. Ideally, congruence should be among the primary criteria for any biogeographic analysis based on species distributions. Unfortunately, until now, most methods have not employed spatial congruence between distributions as an explicit and controllable parameter.
 
 <table>
  <tr>
  <td>
    <img height = "200" src="www/Psophia.jpg">
  </td>
  <td>
    Psophia leucoptera <i>shares its distribution with other birds at the Inambari Endemic Area, SW Amazonia</i>
  </td>
  </tr>
</table>

What about analyse spatial congruence between species, the primary criterion for connections, in a framework based on Network Analysis, using pairwise relationships to detect independent, potentially overlapping spatial patterns while simultaneously describing distribution gradients? Such a framework would address two analytical requirements not previously combined in any method, representing a significant advance in the biogeographical analysis of species distributions.

Well, that is what SCAN does. As a bonus, it brings spatial analysis to its essence in an extremely simple theoretical and conceptual background. No fancy models, statistics, or simulations. Just a fine adjust of the Spatial Congruence between species to find the groups of species that are composing patterns. Saptial patterns seem as network relationships.

<table>
  <tr>
  <td>
    <img height=80% src="www/Primate_chorotypes_graph.png">
  </td>
  <td>
    <i>Examples of Primate Chorotypes across South and Central Americas at a Spatial Congruence Cs = 0.6 - their network structures are show in the right panel</i>
  </td>
  </tr>
</table>

The algorithm applies objective criteria to assess the spatial properties of entities such as raw species distributions, though it can be extrapolated to analyze environments and geographic regions. Recognized patterns may range from highly congruent configurations, where species exhibit very similar distributions and are highly clustered in their graph spatial representations, to assemblages characterized by less overlap and relaxed network spatial connections. When species overlap or replace one another along transitional and ecological zones, these patterns are recognized as liner connections in a network. This flexibility enables the recognition of potentialy dynamic spatial processes and traces of differential responses to evolutionary or ecological filters, while facilitating comparisons between species and regions based on natural and objective criteria, including the number of species in groups, their level of congruence, the ratio of shared to total distributional area, and numerous metrics derived from network analytics.

## 🚀 Installation & Usage
Unlike previous versions, **SCAN V 1.01 runs on a current R version** (tested on R 4.5.2). You do not need to downgrade your R installation.

There are three main options to use SCAN: 
1) Use an online stable version in HuggingFace;
2) Install R / RStudio locally but access the App remotely in GitHub without downloading files (see instructions below).
3) Download or clone this [SCAN folder](https://github.com/cassianogatto/SCAN_R_4.5.2) in your local machine and run ui.R and server.R files directly in R/RStudio;

### 1: Online App
The online version is the easiest way to run [SCAN Analysis App](available at [SCAN Analysis App](https://huggingface.co/spaces/cassianogatto/SCAN_R_4.5.2)). However, there are hosting limitations in terms of memory and computional capacity. This means that the App only works with small sets of species / shapefiles. The low computational power of the host engine may lead to freeze the browser and data get lost. In this case is recomended to use SCAN in your local machine.

### 2: Install R and RStudio
If you haven't already, please download and install the latest versions:
1.  **Download R:** [https://cran.r-project.org/](https://cran.r-project.org/)
2.  **Download RStudio:** [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

#### Install Required Packages
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
  "RColorBrewer",
  "DT"
))
```
### 3: Run SCAN remotelly
Once the packages are installed, you can launch the SCAN Engine directly from GitHub by typing this in your R console:
```r
library(shiny)
# Run SCAN directly from the repository
runGitHub("SCAN_R_4.5.2", "cassianogatto")
```
When you run SCAN remotely for the first time is recommended to downloado the [example](https://github.com/cassianogatto/SCAN_R_4.5.2/examples) folder in this repository. A sample of Neotropical Primate distributions is available to test:
  a) map loading, as the shp files are properly configurated and can be used as a template, 
  b) Spatial Congruence Cs calculus, as the data is small enough for a rapid test of the App, 
  c) SCAN algorithm, and 
  d) Graph and Map interfaces of the SCAN App.

### 4: Clone or download SCAN_R_4.5.2 folder in your computer
Alternatively, you can download or clone [(see github directions)](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) the [SCAN_R_4.5.2](https://github.com/cassianogatto/SCAN_R_4.5.2) directory in your computer.
After downloading the folder open ui.R and server.R in RStudio, call for the Shiny package, and run the App:
```r
library(shiny)
# Run SCAN
shiny::runApp('C:/SCAN_R4.5.2')
```
Or use the "run App" buttom to start the SCAN shiny app in RStudio or your local favorite internet browser.

### Example
The `example` folder <strong>example</strong> brings a small sample of New World Primate distributions to practice and analyze your first Chorotypes!

### Perspectives...
We are applying the method to the analysis of endemic patterns of South American Birds and Primates (with collaborators). SCAN is super intuitive, allows the gattering of new insights about species distributions, and now is fully converted to a standard network analysis (in R). Many network tools and concepts can now be integrated to biogeographical analysis.

The **paper** with concepts is [here](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0245818) 

My **thesis** is also available from [here](https://repositorio.inpa.gov.br/bitstream/1/39803/3/tese_cassiano.pdf) (Intro in Portuguese, Chaps 1 & 2 in English).
