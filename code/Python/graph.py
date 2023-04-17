# Importamos todas las librerías necesarias
import argparse
import pandas as pd
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import os # Para cambiar de directorio
import graphviz as gv # Importar el paquete necesario para obtener los archivos .dot

def getDots(name, labels, edges):
    # Crear un objeto de tipo grafo
    dot = gv.Digraph(comment=name)
    
    # Añadir nodos al grafo
    for ele in labels:
        dot.node(ele,ele)

    # Agregar aristas al grafo
    for i in range(len(edges)):
        for j in range(len(edges)):
            if edges[i][j] == 1:
                dot.edge(labels[i], labels[j], constraint='false')
                
    # Guardamos y renderizamos el código fuente
    dot.render('../doctest-output/'+name+'.gv', view=False)
    
def myFilter(adj_matrix):
    # Transformamos la lista a un numpy array
    matrix = np.array(adj_matrix)
    
    # Obtenemos el número total de elementos en la matriz
    num_elem = matrix.size

    # Ordenamos los elementos de mayor a menor y los aplanamos en un array unidimensional
    sorted_elements = np.sort(matrix.flatten())[::-1]

    # Calculamos la suma acumulada de las frecuencias y la normalizamos
    acumulated_frequencies = np.cumsum(sorted_elements) / np.sum(sorted_elements)

    # Encontramos el índice donde se encuentra el 90% de las frecuencias más altas
    index = np.argmax(acumulated_frequencies >= 0.9)

    # Hacemos cero a todos los elementos que estén por debajo del corte
    matrix[matrix < sorted_elements[index]] = 0
    
    return matrix.tolist()

parser = argparse.ArgumentParser()
parser.add_argument("archivo", help="Ruta del archivo a procesar")
args = parser.parse_args()

# Leemos el dataset
df = pd.read_csv('DatasetFusionadoCompuesto.csv')

# Eliminamos las columnas con valores NaN
df=df.dropna(axis=1)

# Ordenamos los valores según las sesiones
df.sort_values(by="Session")

# Listado de grupos
grupos = df['Grupo'].unique().tolist()

# Cambiamos de directorio
os.chdir('matrices')

for grupo in grupos:
    # Filtrado del dataset
    newdf = df[(df.Grupo == grupo)]
    
    # Cálculo del número de nodos
    num = newdf['Compuesto'].nunique() + 2

    # Creamos una matriz de adyacencia vacía
    adj_matrix = []
    for i in range(num):
        new_row = []
        for j in range(num):
            new_row.append(0)
        adj_matrix.append(new_row)

    # Obtenemos todos los posibles valores de la columna Compuesto y los ordenamos
    labels = newdf['Compuesto'].unique().tolist()
    labels.append('START')
    labels.append('END')

    # Crear diccionario entre labels y posiciones dentro de la lista de labels
    diccionario = {}

    # Iterar sobre la lista
    for indice, elemento in enumerate(labels):
        # Agregar la clave-valor al diccionario
        diccionario[elemento] = indice

    # Cálculo de la matriz de frecuencias
    last_session = -1
    last_label = 'last_label'
    for index, row in newdf.iterrows():
        new_session = row['Session']
        new_label = row['Compuesto']
        if new_session == last_session:
            # Añadir una transición del último label al label actual
            adj_matrix[diccionario[last_label]][diccionario[new_label]] += 1
        else:
            if last_session != -1:
                # Añadir una transición del último label a END
                adj_matrix[diccionario[last_label]][diccionario['END']] += 1
            # Añadir una transición de START al label actual
            adj_matrix[diccionario['START']][diccionario[new_label]] += 1
        last_session = new_session
        last_label = new_label
        
    adj_matrix = myFilter(adj_matrix)

    # Archivo donde se desean guardar los resultados
    file_name = grupo.replace(" ","")

    # Escribir el vector y la matriz en el mismo archivo
    with open(file_name + '.txt', 'w') as f:
        label_text = ''
        for ele in labels:
            label_text = label_text + ele + ' '
        label_text = label_text + '\n'

        # Escribir el vector de las cabeceras en un archivo CSV
        f.write(label_text)

        # Escribir la matriz de adyacencia en un archivo CSV
        np.savetxt(f, adj_matrix, delimiter=' ', fmt='%d')

    # Matriz vacía en la que insertaremos las diferentes aristas
    edges = []

    # Recorrido de la matriz original
    for row in adj_matrix:
        new_row = []
        for element in row:
            if element > 0:
                new_row.append(1)
            else:
                new_row.append(0)
        edges.append(new_row)

    # Crear un grafo vacío
    grafo = nx.DiGraph()

    # Agregar nodos al grafo
    grafo.add_nodes_from(labels)

    # Agregar aristas al grafo
    for i in range(len(edges)):
        for j in range(len(edges)):
            if edges[i][j] == 1:
                grafo.add_edge(labels[i], labels[j], weight=adj_matrix[i][j])

    # Posiciones horizontales
    horizontal = []
    pos_ini = 0
    for i in range(len(labels)-2):
        horizontal.append(pos_ini)
        pos_ini+=2

    # Posiciones verticales
    vertical = {20:-3,40:-6,60:-9,80:-12,100:-15}


    # Definir la posición de los nodos
    max_problem = 0
    pos = {}
    for i in range(len(labels)-2):
        label = labels[i]
        r, j = label.split('-')
        r = int(r[7:])
        if r > max_problem:
            max_problem = r
        j = int(j)
        pos[label] = (horizontal[r], vertical[j])

    pos['START'] = (max_problem,3)
    pos['END'] = (max_problem,-21)

    # Crear una figura de tamaño 10x10 pulgadas
    plt.figure(figsize=(18, 24))

    # Agregamos las etiquetas a los nodos
    etiquetas_nodos = nx.get_node_attributes(grafo, 'label')
    nx.draw_networkx_labels(grafo, pos, etiquetas_nodos, font_size=9, font_family="sans-serif")
    # Agregamos las etiquetas a las aristas
    edge_labels = nx.get_edge_attributes(grafo, "weight")
    nx.draw_networkx_edge_labels(grafo, pos, edge_labels)

    # Dibujar el grafo con posiciones fijas
    nx.draw_networkx(grafo, pos=pos, node_shape='o', node_size=8000, node_color="white", edgecolors="black", linewidths=2)
    # Pedir al usuario nombre del archivo donde guardar la imagen
    image_name = grupo.replace(" ","")
    # Cambiar de directorio
    os.chdir('../graphs')
    # Guardar la imagen
    plt.savefig(image_name + '.png')
    
    getDots(image_name,labels,edges)
    
    # Cambiamos de directorio
    os.chdir('../matrices')