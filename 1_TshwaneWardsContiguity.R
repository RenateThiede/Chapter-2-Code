#2. This is the second step. Obtain adjacent wards.



TshwaneDF <- as.data.frame(nb2mat(poly2nb(ts.wards),style="B"))
rownames(TshwaneDF) <- ts.wards$ADM4_PCODE
colnames(TshwaneDF) <- ts.wards$ADM4_PCODE

#Allow a ward to communicate with itself
for (i in 1:nrow(TshwaneDF)){
  TshwaneDF[i,i]=1
}
