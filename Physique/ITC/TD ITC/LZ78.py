def compression(texte):
    n=len(texte)
    i,code,dc=0,[],{"":0}
    while i<n:
        code,i=compresser(texte,code,dc,i)
    return code, dc



def compresser(texte,code,dc,i):
    n=len(texte)
    assert i<len(texte)
    w=""
    while i<n and w+texte[i] in dc: #on avance sur le préfixe tant qu'il est dans le dictionnaire
        w+=texte[i]
        i+=1
    if i<n:
        #le préfixe w est dans le dictionnaire
        #le nouveau préfixe w1=w+texte[i] n'y figure pas et y est ajouté 
        p=len(dc) #la position est donnée par la longueur du dictionnaire
        c=texte[i]
        w1=w+c
        dc[w1]=p
        code.append((dc[w],c)) 
    return code,i+1
            



def decompression(code,dc):
    dci={nvcle:nvval for (nvval,nvcle) in dc.items()} #construction du dictionnaire inversé
    texte=""
    print("Dictionnaire inversé:",dci)
    for el in code:
        w=dci[el[0]]
        s=el[1]
        texte+=w+s
    return texte







texte="ABRACADABRA"
code,dc=compression(texte)
print("Code:",code)
print("Dictionnaire:",dc)




print("Résultat de la décompression:",decompression(code,dc))



    
    
          