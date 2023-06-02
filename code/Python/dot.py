import math

def getDotsCompound(name, labels, edges):
    # Inicializar el diccionario de frecuencia de nodos
    freq = []
    for row in edges:
        freq.append(0);

    # Recorrer cada fila de la matriz
    for i, row in enumerate(edges):
        # Recorrer cada columna de la fila
        for col in range(len(row)):
            # Si hay una arista entre los nodos, aumentar la frecuencia de ambos nodos en 1
            if row[col] >= 1:
                # Aumentar la frecuencia del nodo actual en row[col]
                freq[col] += row[col]
    
    # Recortar los labels
    for i in range(len(labels)):
        labels[i] = labels[i].replace('PROBLEM','P')
    
    with open(name+'.dot', 'w') as f:
        f.write('digraph graphname {\n\tdpi = 150\n\tsize="16,11!";\n\tmargin = 0;\n')
        for i in range(len(labels)):
            color = 'aqua'
            if freq[i] >= 10 and freq[i] < 20:
                color = 'lightskyblue'
            elif freq[i] >= 20 and freq[i] < 30:
                color = 'deepskyblue'
            elif freq[i] >= 30 and freq[i] < 40:
                color = 'dodgerblue'
            elif freq[i] >= 40:
                color = 'royalblue'
            if labels[i] == 'START' or labels[i] == 'END':
                color = 'white'
            
            f.write('"' + labels[i] + '"' + ' [shape=plain, label=<<table border="0" cellborder="1" cellspacing="0"><tr><td bgcolor="' + color + '"><FONT face="Arial" POINT-SIZE="10"><b>' + labels[i] + '</b></FONT></td></tr>')
            f.write('<tr><td bgcolor="white"><FONT face="Arial" POINT-SIZE="8"><i>' + str(freq[i]) + '</i></FONT></td></tr></table>>]\n')
        # Agregar aristas al grafo
        for i in range(len(edges)):
            for j in range(len(edges)):
                if edges[i][j] >= 1:
                    if labels[i] == "START" or labels[j] ==  "END":
                        f.write('"' + labels[i] + '" -> "' + labels[j] + '" [ style = dashed color=grey label ="' + str(edges[i][j]) + '" labelfloat=false fontname="Arial" fontsize=8]\n')
                    else:
                        f.write('"' + labels[i] + '" -> "' + labels[j] + '" [ color=grey16 penwidth = "' + str(max(1,math.log(edges[i][j]))) + '"label ="' + str(edges[i][j]) + '" labelfloat=false fontname="Arial" fontsize=8]\n')
        f.write('}')
        
def getDotsProblem(name, labels, edges):
    color = {
        'P1': 'greenyellow',
        'P1 OK': 'greenyellow',
        'P1 FAIL': 'greenyellow',
        'P2': 'bisque',
        'P2 OK': 'bisque',
        'P2 FAIL': 'bisque',
        'P3': 'cadetblue',
        'P3 OK': 'cadetblue',
        'P3 FAIL': 'cadetblue',
        'P4': 'orange',
        'P4 OK': 'orange',
        'P4 FAIL': 'orange',
        'P5': 'deepskyblue',
        'P5 OK': 'deepskyblue',
        'P5 FAIL': 'deepskyblue',
        'P6': 'gold',
        'P6 OK': 'gold',
        'P6 FAIL': 'gold',
        'P7': 'hotpink',
        'P7 OK': 'hotpink',
        'P7 FAIL': 'hotpink',
        'P8': 'indianred1',
        'P8 OK': 'indianred1',
        'P8 FAIL': 'indianred1',
        'P9': 'mediumpurple1',
        'P9 OK': 'mediumpurple1',
        'P9 FAIL': 'mediumpurple1'
    }
    
    # Inicializar el diccionario de frecuencia de nodos
    freq = []
    for row in edges:
        freq.append(0);

    # Recorrer cada fila de la matriz
    for i, row in enumerate(edges):
        # Recorrer cada columna de la fila
        for col in range(len(row)):
            # Si hay una arista entre los nodos, aumentar la frecuencia de ambos nodos en 1
            if row[col] >= 1:
                # Aumentar la frecuencia del nodo actual en row[col]
                freq[col] += row[col]
    
    with open(name+'.dot', 'w') as f:
        f.write('digraph graphname {\n\tdpi = 150\n\tsize="16,11!";\n\tmargin = 0;\n')
        for i in range(len(labels)):
            if labels[i] == 'START' or labels[i] == 'END':
                f.write('"' + labels[i] + '"' + ' [shape=box, fillcolor=white, style=filled, color=black]')
            else:
                if labels[i].endswith("FAIL"):
                    f.write('"' + labels[i] + '"' + ' [shape=circle, color=' + color[labels[i]] + ', peripheries=2, style=filled]')
                else:
                    f.write('"' + labels[i] + '"' + ' [shape=circle, color=' + color[labels[i]] + ', style=filled]')
        # Agregar aristas al grafo
        for i in range(len(edges)):
            for j in range(len(edges)):
                if edges[i][j] >= 1:
                    if labels[i] == "START" or labels[j] ==  "END":
                        f.write('"' + labels[i] + '" -> "' + labels[j] + '" [ style = dashed color=grey label ="' + str(edges[i][j]) + '" labelfloat=false fontname="Arial" fontsize=8]\n')
                    else:
                        f.write('"' + labels[i] + '" -> "' + labels[j] + '" [ color=grey16 penwidth = "' + str(max(1,math.log(edges[i][j]))) + '"label ="' + str(edges[i][j]) + '" labelfloat=false fontname="Arial" fontsize=8]\n')
        f.write('}')