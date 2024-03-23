
?embed

# {y_t, y_(t-1), y_(t-2)} 등 y_t와 lagged y_t들을 만드는 것 

x1 = cbind(seq(1,10))
x1
embed(x1,4)
embed(x1,6)                                   

x2 = cbind(seq(1,10), seq(11,20), seq(21,30))
x2
embed(x2,2)
embed(x2,3)
embed(x2,4)
