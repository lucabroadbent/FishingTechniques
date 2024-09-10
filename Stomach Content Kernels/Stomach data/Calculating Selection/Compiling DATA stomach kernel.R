#making the stomach kernel file

blue whiting
alpha ll ul lr ur ## -0.1310297 2.3405346 22.9956791 9.0047846 1.4277256 
fit=ok


whiting
-0.3145214 3.6304283 4.5452752 13.3696362 1.6451461 
fit = not good

cod
-0.0005523561 3.7634229256 2.2665059865 11.7093958978 1.8644950972 
fit =ok

common dab
-0.05424663 3.39867067 2.29557044 9.44078251 1.66081495 
fit = very good.

european hake
## $pro ## [1] 0.6450399 0.3549601 
## ## $mean ## 1 2 ## 4.286325 4.710199 ## 
## $variance ## $variance$modelName ## [1] "V" ## 
## $variance$d ## [1] 1 ## ## $variance$G ## [1] 2 ## 
## $variance$sigmasq ## [1] 0.06341901 2.44856948 ## 
## $variance$scale ## [1] 0.06341901 2.44856948 
fit = good

haddock
0.4805395 3.8865861 2.1827656 11.7890770 2.3939047 
fit=good

herring
no fit 

Horse Mackerel
-0.2374178 3.5785207 1.9266416 13.1902886 3.3245700 
fit = good

Mackerel
-0.2856444 3.7936324 2.0442341 16.1008630 2.7353730 
fit=very good

Megrim

## $pro ## [1] 0.5504123 0.4495877 
## ## $mean ## 1 2 ## 4.015029 5.947145
## ## $variance ## $variance$modelName 
## [1] "V" ## ## $variance$d 
## [1] 1 ## ## $variance$G 
## [1] 2 ## ## $variance$sigmasq 
## [1] 0.2139633 2.8654756 ## 
## $variance$scale ## [1] 0.2139633 2.8654756 

fit = ok

monkfish
0.0700759 0.9805321 54.0317054 5.3291042 4.3538944 
fit=not good

norway pout
-0.6399368 4.0059442 1.7483370 11.8681193 13.6461300 
fit=ok

plaice
## $pro ## [1] 0.4013671 0.5986329 ## 
## $mean ## 1 2 ## 5.650236 7.762250 ## 
## $variance ## $variance$modelName ## [1] "V" ## 
## $variance$d ## [1] 1 ## ## $variance$G ## [1] 2 ## 
## $variance$sigmasq ## [1] 0.1959833 5.8912814 ##
## $variance$scale ## [1] 0.1959833 5.8912814 

fit = ok

poor cod
0.03769714 3.89995523 1.84539297 11.07162811 9.65116209 
fit = good

sole
## $pro ## [1] 0.370072 0.629928 ## 
## $mean ## 1 2 ## 5.256386 8.947248 ##
## $variance ## $variance$modelName ## [1] "V" ## 
## $variance$d ## [1] 1 ## ## $variance$G ## [1] 2 ##
## $variance$sigmasq ## [1] 0.820242 2.179852 ##
## $variance$scale ## [1] 0.820242 2.179852 
fit=very good


Sprat - weird numbers 

## $pro ## [1] 0.2307388 0.4959397 0.2733215 ## 
## $mean ## 1 2 3 ## 3.943746 8.584915 12.039642 ## 
## $variance ## $variance$modelName ## [1] "E" ##
## $variance$d ## [1] 1 ## ## $variance$G ## [1] 3 ## 12
## $variance$sigmasq ## [1] 0.7068256 
fit =ok


spurdog

## $pro ## [1] 0.377416 0.622584 ##
## $mean ## 1 2 ## 3.787757 6.772024 ##
## $variance ## $variance$modelName ## [1] "V" ##
## $variance$d ## [1] 1 ## ## $variance$G ## [1] 2 ##
## $variance$sigmasq ## [1] 0.1022199 4.8450106 ## 12 
## $variance$scale ## [1] 0.1022199 4.8450106

fit=not good


species_data <- list(
  "blue whiting" = list(
    fit = "ok",
    parameters = c(alpha = -0.1310297, ll = 2.3405346, ul = 22.9956791, lr = 9.0047846, ur = 1.4277256)
  ),
  "whiting" = list(
    fit = "not good",
    parameters = c(alpha = -0.3145214, ll = 3.6304283, ul = 4.5452752, lr = 13.3696362, ur = 1.6451461)
  ),
  "cod" = list(
    fit = "ok",
    parameters = c(alpha = -0.0005523561, ll = 3.7634229256, ul = 2.2665059865, lr = 11.7093958978, ur = 1.8644950972)
  ),
  "common dab" = list(
    fit = "very good",
    parameters = c(alpha = -0.05424663, ll = 3.39867067, ul = 2.29557044, lr = 9.44078251, ur = 1.66081495)
  ),
  "european hake" = list(
    fit = "good",
    parameters = list(
      pro = c(0.6450399, 0.3549601),
      mean = c(4.286325, 4.710199),
      sigmasq = c(0.06341901, 2.44856948)
    )
  ),
  "haddock" = list(
    fit = "good",
    parameters = c(alpha = 0.4805395, ll = 3.8865861, ul = 2.1827656, lr = 11.7890770, ur = 2.3939047)
  ),
  "herring" = list(
    fit = "no fit",
    parameters = NA
  ),
  "Horse Mackerel" = list(
    fit = "good",
    parameters = c(alpha = -0.2374178, ll = 3.5785207, ul = 1.9266416, lr = 13.1902886, ur = 3.3245700)
  ),
  "Mackerel" = list(
    fit = "very good",
    parameters = c(alpha = -0.2856444, ll = 3.7936324, ul = 2.0442341, lr = 16.1008630, ur = 2.7353730)
  ),
  "Megrim" = list(
    fit = "ok",
    parameters = list(
      pro = c(0.5504123, 0.4495877),
      mean = c(4.015029, 5.947145),
      sigmasq = c(0.2139633, 2.8654756)
    )
  ),
  "monkfish" = list(
    fit = "not good",
    parameters = c(alpha = 0.0700759, ll = 0.9805321, ul = 54.0317054, lr = 5.3291042, ur = 4.3538944)
  ),
  "norway pout" = list(
    fit = "ok",
    parameters = c(alpha = -0.6399368, ll = 4.0059442, ul = 1.7483370, lr = 11.8681193, ur = 13.6461300)
  ),
  "plaice" = list(
    fit = "ok",
    parameters = list(
      pro = c(0.4013671, 0.5986329),
      mean = c(5.650236, 7.762250),
      sigmasq = c(0.1959833, 5.8912814)
    )
  ),
  "poor cod" = list(
    fit = "good",
    parameters = c(alpha = 0.03769714, ll = 3.89995523, ul = 1.84539297, lr = 11.07162811, ur = 9.65116209)
  ),
  "sole" = list(
    fit = "very good",
    parameters = list(
      pro = c(0.370072, 0.629928),
      mean = c(5.256386, 8.947248),
      sigmasq = c(0.820242, 2.179852)
    )
  ),
  "Sprat" = list(
    fit = "ok",
    parameters = list(
      pro = c(0.2307388, 0.4959397, 0.2733215),
      mean = c(3.943746, 8.584915, 12.039642),
      sigmasq = 0.7068256
    )
  ),
  "spurdog" = list(
    fit = "not good",
    parameters = list(
      pro = c(0.377416, 0.622584),
      mean = c(3.787757, 6.772024),
      sigmasq = c(0.1022199, 4.8450106)
    )
  )
)


sort(unique(stom_df$pred_taxa))

art <- stom_df%>%filter(pred_taxa=="Sardina pilchardus")
art2 <- stom_df%>%filter(pred_taxa=="Argentinidae")

art <- repeat_elements(art2$sample_id, art2$nprey_perpred)
