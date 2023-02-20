#  A -  A

#centrando A e normalizzando A

A=matrix(c( -0.03,  0.47,
            -0.67, -0.06,
             0.59, -0.22,
            -0.23, -0.35,
             0.35,  0.40,
             0.16, -0.47,
            -0.10,  0.47),ncol=2,nrow = 7,byrow = TRUE)

B=matrix(c(  -0.60, 0.48,
             -0.32, 0.39,
             -0.08, 0.38,
              0.12, 0.37,
              0.29, 0.36,
              0.41, 0.35,
              0.51, 0.30),ncol=2,nrow = 7,byrow = TRUE)

C=matrix(c( 0.36,
            0.35,
            0.34,
            0.33,
            0.32,
            0.31,
            0.30,
            0.30,
            0.28,
            0.27),ncol=1,nrow = 10,byrow = TRUE)

#Rotated core
#B1xC1   B2xC1
#A1  -12.41    0.01
#A2    0.00  -17.19

Core=matrix(c(-12.41 ,0.01,0,-17.19),ncol=2)

# Si sfrutta il fatto che P*Q=R=2
# Poiche'¨ c'e' una sola componente in C, vale la seguente uguaglianza
M1=A%*%Core%*%kronecker(t(C),t(B))
M2=kronecker(t(C),A%*%Core%*%t(B))

# check uguaglianza
sum(abs(M1-M2))
#3.808065e-14


# attenzione su A%*%Core%*%t(B), matrice di rango 2
# si applica la svd/pca
S=svd(A%*%Core%*%t(B))

# si trovano le nuove matrici A e B (che non sono ancora quelle finali)
A.new=S$u[,1:2]%*%diag(S$d[1:2])
B.new=S$v[,1:2]

# check nessuna perdita di informazione (poiche' rango 2)
sum(abs(A%*%Core%*%t(B)-A.new%*%t(B.new)))
#6.228351e-14

#########################################################

# caso 1: struttua semplice (varimax) su B
vB=varimax(B.new)
B.final1=vB$loadings
A.final1=A.new%*%solve(t(vB$rotmat))

# check stesso fit con A.new-A.new e A.final1-B.final1
sum(abs(A%*%Core%*%t(B)-A.final1%*%t(B.final1)))
#6.705747e-14

# stessa C iniziale
C.final1=C
# core semplice uguale a matrice identita'
Core.final1=c(1,0,0,1)

write.table(A.final1,file=("/Users/flaviolaudenzi/Desktop/A.final1.csv")) 
write.table(B.final1,file=("/Users/flaviolaudenzi/Desktop/B.final1.csv")) 
write.table(C.final1,file=("/Users/flaviolaudenzi/Desktop/C.final1.csv")) 

#####################################################

# caso 2: struttua semplice (varimax) su A

vA=varimax(A.new)
A.final2=vA$loadings
B.final2=B.new%*%solve(t(vA$rotmat))
# check stesso fit con A.new-B.new e A.final2-B.final2
sum(abs(A%*%Core%*%t(B)-A.final2%*%t(B.final2)))
#6.283862e-14
# stessa C iniziale
C.final2=C
# core semplice uguale a matrice identita'
Core.final2=c(1,0,0,1)

write.table(A.final2,file=("/Users/flaviolaudenzi/Desktop/A.final2.csv")) 
write.table(B.final2,file=("/Users/flaviolaudenzi/Desktop/B.final2.csv")) 
write.table(C.final2,file=("/Users/flaviolaudenzi/Desktop/C.final2.csv")) 
