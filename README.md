# utl-r-python-packages-that-that-can-estimate-the-optimum-number-of-clusters
R python packages that that can estimate the optimum number of clusters
    %let pgm=utl-r-python-packages-that-that-can-estimate-the-optimum-number-of-clusters;

    %stop_submission;

    R python packages that that can estimate the optimum number of clusters

    PROBLEM: Estimate the optimum number of clusters

    Too long to post here see github

    I like R for this type of problem

    I dont think sas 'proc klus' will run in classic sas stat, without a cas server?
    CAS datasets are less likely to integrate with other languages.

    gihub (see files for graphics)
    https://tinyurl.com/ykr7ajks
    https://github.com/rogerjdeangelis/utl-r-python-packages-that-that-can-estimate-the-optimum-number-of-clusters

    Altair Community
    https://tinyurl.com/mwz3h38e
    https://community.altair.com/discussion/64393/please-can-proc-kclus-be-added-to-altair-slc?tab=all#latest


    R and PYTHON have many packages for clustering.
    Often R packages employ newer algorithms before sas.

    R
    1 library(NbClust)
    2 library(ClusterR)
    3 library(cluster)
    4 library(stats)
      kmeans()
      hclust()
    5 library(factoexta) for visualization
      fviz_cluster(): Visualizes K-Means and PAM results.
      fviz_dend(): Enhances dendrogram plots from hclust.
      fviz_nbclust(): Helps determ

    PYTHON
    1 sklearn.cluster
    2 pyclustering
    3 AgglomerativeClustering
    4 hdbscan

    /*                   _
    (_)_ __  _ __  _   _| |_
    | | `_ \| `_ \| | | | __|
    | | | | | |_) | |_| | |_
    |_|_| |_| .__/ \__,_|\__|
            |_|
    */

    data sd1.baseball;
    informat
    TEAM $14.
    CRATBAT 8.
    CRHITS 8.
    CRRUNS 8.
    POSITION $8.;
    input
      TEAM CRATBAT CRHITS CRRUNS POSITION @@;
    cards4;
    Houston 3449 835 321 C    Houston 2079 565 258 SS   Toronto 3558 928 513 CF
    Seattle 1624 457 224 1B   Houston 2133 594 287 3B   Detroit 4273 1123 577 C
    Oakland 4408 1133 501 SS  Toronto 2303 571 266 C    Detroit 4704 1320 724 2B
    Atlanta 341 86 32 SS      Toronto 2129 610 297 LF   Oakland 2051 549 300 RF
    Detroit 4631 1300 702 SS  Houston 985 260 148 1B    Oakland 498 116 59 C
    Oakland 5233 1478 643 1B  Atlanta 3573 866 429 2B   Chicago 1038 271 129 SS
    Chicago 426 109 55 O1     Toronto 2140 568 216 32   Atlanta 4992 1257 699 RF
    Chicago 1931 491 291 CF   Chicago 6986 1972 1070 LF Atlanta 1493 359 176 C
    Houston 2308 633 349 2B   Seattle 4677 1051 681 DH  Seattle 1556 470 245 LF
    Houston 591 149 80 CF     Chicago 1649 453 211 1B   Houston 5885 1543 751 3B
    Atlanta 3571 994 545 1B   Chicago 3754 1077 492 RF  Detroit 1257 329 166 OF
    Seattle 1309 308 126 C    Seattle 618 129 72 2B     Chicago 7058 1845 965 3B
    Atlanta 2516 684 371 OF   Toronto 2325 634 371 RF   Chicago 1770 408 238 DH
    Chicago 6521 1767 1003 C  Chicago 440 103 67 LF     Toronto 912 234 102 DO
    Toronto 3945 1016 539 DH  Oakland 696 173 101 LF    Toronto 2288 614 295 3B
    Oakland 4478 1307 634 3B  Houston 7472 2147 980 LF  Atlanta 3358 882 365 S3
    Detroit 5150 1429 747 CF  Chicago 3859 916 557 2B   Chicago 3146 902 494 2B
    Houston 3742 968 409 SS   Chicago 2641 671 273 C    Seattle 290 80 27 C
    Chicago 6631 1634 698 3S  Detroit 4040 1130 544 DH  Chicago 831 210 106 SS
    Oakland 7117 1981 964 OF  Chicago 1568 408 202 UT   Seattle 1716 403 211 SS
    Chicago 514 120 57 CF     Seattle 670 167 89 CF     Detroit 2658 657 324 UT
    Detroit 815 205 99 3B     Chicago 4618 1330 616 OF  Toronto 1518 448 196 SS
    Detroit 4484 1231 612 LF  Seattle 1437 377 181 3B   Atlanta 1337 339 135 OF
    Detroit 7761 1947 1175 1B Chicago 450 108 38 C      Chicago 927 227 106 3B
    Toronto 3651 1046 461 2B  Oakland 707 179 77 C      Oakland 1546 397 226 2B
    Seattle 2174 555 285 OF   Houston 1689 462 219 RF   Houston 4086 1150 579 OF
    Oakland 1064 290 123 23   Detroit 2723 750 433 RF   Atlanta 8396 2402 1048 UT
    Oakland 6677 1575 901 DH  Chicago 3082 880 363 RF   Chicago 1700 433 217 3B
    Chicago 6311 1661 1019 3O Atlanta 3423 970 408 3B   Toronto 3198 857 470 1B
    Atlanta 5017 1388 813 CF  Seattle 911 214 150 DH
    Oakland 3828 948 575 CF   Chicago 3006 844 436 1B
    Seattle 592 164 87 RF     Detroit 4479 1222 557 OF
    ;;;;
    run;quit;

    /*
     _ __  _ __ ___   ___ ___  ___ ___
    | `_ \| `__/ _ \ / __/ _ \/ __/ __|
    | |_) | | | (_) | (_|  __/\__ \__ \
    | .__/|_|  \___/ \___\___||___/___/
    |_|
    */

    &_init_;
    libname sd1 sas7bdat "d:/sd1";
    proc r;
    export data=sd1.baseball r=baseball;
    submit;
    library(NbClust)
    library(factoextra)
    library(tidyverse)

    # Standardize data
    baseball_scaled <- scale(select_if(baseball, is.numeric))

    # Automatic cluster number determination
    set.seed(123)
    nb_result <- NbClust(baseball_scaled, method = "kmeans")

    # Get optimal number of clusters
    optimal_k <- nb_result$Best.nc[1]

    # Final clustering
    final_clusters <- kmeans(baseball_scaled, centers = optimal_k, nstart = 25)

    # Detailed results
    summary(final_clusters)
    endsubmit;
    ;quit;run;

    /*           _               _
      ___  _   _| |_ _ __  _   _| |_
     / _ \| | | | __| `_ \| | | | __|
    | (_) | |_| | |_| |_) | |_| | |_
     \___/ \__,_|\__| .__/ \__,_|\__|
                    |_|
    */

    Altair SLC

    * According to the majority rule, the best number of clusters is  3

    *** : The Hubert index is a graphical method of determining the number of clusters.
                    In the plot of Hubert index, we seek a significant knee that corresponds to a
                    significant increase of the value of the measure i.e the significant peak in Hubert
                    index second differences plot.

    *** : The D index is a graphical method of determining the number of clusters.
                    In the plot of D index, we seek a significant knee (the significant peak in Dindex
                    second differences plot) that corresponds to a significant increase of the value of
                    the measure.

    *******************************************************************
    * Among all indices:
    * 5 proposed 2 as the best number of clusters
    * 9 proposed 3 as the best number of clusters
    * 3 proposed 5 as the best number of clusters
    * 2 proposed 8 as the best number of clusters
    * 1 proposed 11 as the best number of clusters
    * 1 proposed 12 as the best number of clusters
    * 1 proposed 14 as the best number of clusters
    * 1 proposed 15 as the best number of clusters
                       ***** Conclusion *****

    * According to the majority rule, the best number of clusters is  3


    *******************************************************************
                 Length Class  Mode
    cluster      93     -none- numeric
    centers      45     -none- numeric
    totss         1     -none- numeric
    withinss     15     -none- numeric
    tot.withinss  1     -none- numeric
    betweenss     1     -none- numeric
    size         15     -none- numeric
    iter          1     -none- numeric
    ifault        1     -none- numeric

    /*
    | | ___   __ _
    | |/ _ \ / _` |
    | | (_) | (_| |
    |_|\___/ \__, |
             |___/
    */

    2037      ODS _ALL_ CLOSE;
    2038      FILENAME WPSWBHTM TEMP;
    NOTE: Writing HTML(WBHTML) BODY file d:\wpswrk\_TD24756\#LN00062
    2039      ODS HTML(ID=WBHTML) BODY=WPSWBHTM GPATH="d:\wpswrk\_TD24756";
    2040
    2041      &_init_;
    2042      libname sd1 sas7bdat "d:/sd1";
    NOTE: Library sd1 assigned as follows:
          Engine:        SAS7BDAT
          Physical Name: d:\sd1

    2043      proc r;
    NOTE: Using R version 4.5.1 (2025-06-13 ucrt) from d:\r451
    2044      export data=sd1.baseball r=baseball;
    NOTE: Creating R data frame 'baseball' from data set 'SD1.baseball'

    2045      submit;
    2046      library(NbClust)
    2047      library(factoextra)
    2048      library(tidyverse)
    2049
    2050      # Standardize data
    2051      baseball_scaled <- scale(select_if(baseball, is.numeric))
    2052
    2053      # Automatic cluster number determination
    2054      set.seed(123)
    2055      nb_result <- NbClust(baseball_scaled, method = "kmeans")
    2056
    2057      # Get optimal number of clusters
    2058      optimal_k <- nb_result$Best.nc[1]
    2059
    2060      # Final clustering
    2061      final_clusters <- kmeans(baseball_scaled, centers = optimal_k, nstart = 25)
    2062
    2063      # Detailed results
    2064      summary(final_clusters)
    2065      endsubmit;

    NOTE: Submitting statements to R:

    > library(NbClust)
    Loading required package: ggplot2
    Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
    > library(factoextra)
    -- [1mAttaching core tidyverse packages
    >
    > # Standardize data
    > baseball_scaled <- scale(select_if(baseball, is.numeric))
    >
    > # Automatic cluster number determination
    > set.seed(123)
    > nb_result <- NbClust(baseball_scaled, method = "kmeans")
    >
    > # Get optimal number of clusters
    > optimal_k <- nb_result$Best.nc[1]
    >
    > # Final clustering
    > final_clusters <- kmeans(baseball_scaled, centers = optimal_k, nstart = 25)
    >
    > # Detailed results

    NOTE: Processing of R statements complete
    NOTE: Successfully written image d:\wpswrk\_TD24756\R Plot 1.jpeg
    NOTE: Successfully written image d:\wpswrk\_TD24756\ODS LISTING images\I0000001.jpeg
    NOTE: Successfully written image d:\wpswrk\_TD24756\R Plot 21.jpeg
    NOTE: Successfully written image d:\wpswrk\_TD24756\ODS LISTING images\I0000002.jpeg

    > summary(final_clusters)
    2066      ;quit;run;
    NOTE: Procedure r step took :
          real time : 5.515
          cpu time  : 0.046


    2067
    2068
    2069      quit; run;
    2070      ODS _ALL_ CLOSE;
    2071      FILENAME WPSWBHTM CLEAR;

    /*              _
      ___ _ __   __| |
     / _ \ `_ \ / _` |
    |  __/ | | | (_| |
     \___|_| |_|\__,_|

    */
