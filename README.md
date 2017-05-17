---
title: "Single Sample Gene Enrichment Analysis Taskforce"
output: 
    html_document:
      toc: true # table of content true
      depth: 3  # upto three depths of headings (specified by #, ## and ###)
      number_sections: true  ## if you want number sections at each table header
      theme: cerulean  #
      highlight: tango  # specifies the syntax highlighting style
author: "Ozan Aygun"
date: "May 2017"
---


# Introduction

ssGSEA taskforce is a standalone desktop software for running Single Sample Gene Enrichment Analysis (ssGSEA) and analyzing ssGSEA results. This tutorial is intended for user who would like to quickly get started by installing the software, running ssGSEA analysis and analyzing their results.

<p align="center">
<img src="Figures/ssGSEAtaskforce.jpg" width="300">
</p>

# Download and Software Installation

**Currently available for Windows**

The latest version of the software is available to download from the following URL:


[**DOWNLOAD SSGSEA Taskforce V.0.1**](https://www.dropbox.com/s/3p0mwe38hfutxwj/ssGSEAtaskforce_setup.exe?dl=0)


___

Download the **ssGSEAtaskforce_setup.exe** file into your computer and double click to start the install wizard:

<p align="center">
<img src="Figures/Capture8.JPG" width="500">
</p>



Click to **Next**:

<p align="center">
<img src="Figures/Capture9.JPG" width="500">
</p>


By default, the software will be installed into your C: drive. Click to **INSTALL** to continue.

<p align="center">
<img src="Figures/Capture11.JPG" width="500">
</p>

Complete installation:

<p align="center">
<img src="Figures/Capture12.JPG" width="500">
</p>

You should see the ssGSEA icon on your desktop. 

<p align="center">
<img src="Figures/Capture13.JPG" width="300">
</p>

# Main Menu

Click to run the software. A portable version of Google Chrome will open. You will notice it takes some time to load the software in the first instance,as it may ask you to load some extensions of the Adobe Reader in a seperate tab. You can ignore and close that tab. The actual software will appear in the tab with a default IP address/port: **//127.0.0.1:8888/**

The main menu will look like:

<p align="center">
<img src="Figures/Capture1.JPG" width="600">
</p>

# Run ssGSEA

Click to **Run ssGSEA** to open ssGSEA run wizard:

<p align="center">
<img src="Figures/Capture6.JPG" width="600">
</p>

## Gene expression data in gct 1.2 format

ssGSEA Taskforce currently supports the gct 1.2 input file format. You will be required to convert your gene expression data set into this format, which is straightforward. 

Here is an intiutive example how an example file looks like:

<p align="center">
<img src="Figures/Capture14.JPG" width="600">
</p>

You can prepare your expression file in any software or programming environment you wish. The important points are:

- Give unique names to your treatments (rule of thumb: avoid spaces special characters other than numbers)
- The first two rows are reserved. The left 'corner' has the **#1.2** identifier.
- The second row has to specify the number of Genes in your data set(number of observations) and the number of Treatments, respectively.
- The third row will contain the header.
- The first columns are reserved for gene names: currently supporting official human Gene Symbols, which will become the **Name** and **Description** columns, respectively. Treatment expression data should be listed afterwards.



Once your parameters are ready, click to move to next stage:

<p align="center">
<img src="Figures/Capture7.JPG" width="600">
</p>


**DO NOT CLOSE YOUR BROWSER WHEN YOUR ANALYSIS IS RUNNING!**

Once your run is completed, you will be directed back to the main menu.

<p align="center">
<img src="Figures/Capture1.JPG" width="600">
</p>

# Analyze ssGSEA

Click to **Analyze ssGSEA** to open analysis tools:

<p align="center">
<img src="Figures/Capture2.JPG" width="600">
</p>




