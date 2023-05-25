# Importamos todas las librerías necesarias
import argparse
import pandas as pd
import numpy as np
import os # Para cambiar de directorio
import subprocess
import csv
from dot import *

def remove(adj_matrix):
    cycles = []
 
    # Function to mark the vertex with
    # different colors for different cycles
    def dfs_cycle(u, p, color: list, par: list):
        nonlocal cyclenumber

        # already (completely) visited vertex.
        if color[u] == 2:
            return

        # seen vertex, but was not
        # completely visited -> cycle detected.
        # backtrack based on parents to
        # find the complete cycle.
        if color[u] == 1:
            v = []
            cur = p
            v.append(cur)

            # backtrack the vertex which are
            # in the current cycle thats found
            while cur != u:
                cur = par[cur]
                v.append(cur)
            cycles.append(v)
            cyclenumber += 1

            return

        par[u] = p

        # partially visited.
        color[u] = 1

        # simple dfs on graph
        for index, v in enumerate(adj_matrix[u]):
            if v > 0:
                # if it has not been visited previously
                if index == par[u]:
                    continue
                dfs_cycle(index, u, color, par)

        # completely visited.
        color[u] = 2
        
    
    # arrays required to color the
    # graph, store the parent of node
    color = [0] * len(adj_matrix)
    par = [0] * len(adj_matrix)

    # store the numbers of cycle
    cyclenumber = 0

    # call DFS to mark the cycles
    dfs_cycle(0, len(adj_matrix), color, par)

    for cycle in cycles:
        adj_matrix[cycle[0]][cycle[-1]] = 0
        
    for i in range(len(adj_matrix)):
        for j in range(len(adj_matrix)):
            if adj_matrix[i][j] > 0 and adj_matrix[j][i] > 0:
                adj_matrix[j][i] = 0
    
    return adj_matrix

def deleteZeros(matrix, labels):
    matrix = np.array(matrix)
    labels = np.array(labels)
    
    columns = ~np.all(matrix == 0, axis=0)
    if not columns[len(matrix)-2]:
        columns[len(matrix)-2] = True
    if not columns[len(matrix)-1]:
        columns[len(matrix)-1] = True
        
    rows = ~np.all(matrix == 0, axis=1)
    
    for i in range(len(columns)):
        if not columns[i] and len(labels[i].split('-')) > 1:
            if labels[i].split('-')[1] != '20':
                for j in range(len(columns)):
                    matrix[i][j] = 0
                
    for i in range(len(matrix)-2):
        erase = True
        for j in range(len(matrix)-2):
            if matrix[i][j] > 0:
                erase = False
                break
            elif matrix[j][i] > 0:
                erase = False
                break
        if erase:
            matrix[i][len(matrix)-2] = 0;
            matrix[i][len(matrix)-1] = 0;
            matrix[len(matrix)-1][i] = 0;
            matrix[len(matrix)-2][i] = 0;
            
    columns2 = ~np.all(matrix == 0, axis=0)
    if not columns2[len(matrix)-2]:
        columns2[len(matrix)-2] = True
    if not columns2[len(matrix)-1]:
        columns2[len(matrix)-1] = True
        
    rows2 = ~np.all(matrix == 0, axis=1)

    result = [columns2[i] or rows2[i] for i in range(len(columns))]
    
    matrix_without_zero_columns = matrix[:, result]
    matrix = matrix_without_zero_columns[result, :]
    
    labels = labels[result]
    
    return matrix.tolist(), labels.tolist()
    
def myFilter(adj_matrix):
    # Transformamos la lista a un numpy array
    matrix = np.array(adj_matrix)
    matrix = matrix[:-2,:-2]
    
    # Obtenemos el número total de elementos en la matriz
    num_elem = matrix.size

    # Ordenamos los elementos de mayor a menor y los aplanamos en un array unidimensional
    sorted_elements = np.sort(matrix.flatten())[::-1]

    # Calculamos la suma acumulada de las frecuencias y la normalizamos
    if np.sum(sorted_elements) > 0:
        acumulated_frequencies = np.cumsum(sorted_elements) / np.sum(sorted_elements)

        # Encontramos el índice donde se encuentra el 90% de las frecuencias más altas
        if np.size(acumulated_frequencies) > 0:
            index = np.argmax(acumulated_frequencies >= 0.9)

            # Hacemos cero a todos los elementos que estén por debajo del corte
            matrix[matrix < sorted_elements[index]] = 0

            for i in range(matrix.shape[0]):
                for j in range(matrix.shape[1]):
                    adj_matrix[i][j] = matrix[i][j]
    
    return adj_matrix

def removeCycles(matrix):
    # Recorrer la diagonal de la matriz y asignar un valor de cero en cada posición
    for i in range(len(matrix)):
        matrix[i][i] = 0
        
    for i in range(len(matrix)-2):
        for j in range(len(matrix)-2):
            if matrix[i][j] != 0:
                matrix[j][i] = 0
    
    return matrix

def process(newdf, file_name, image_name, matrices, graphs, dot, activityId, caseId):
    # Cálculo del número de nodos
    num = newdf[activityId].nunique() + 2
    
    # Creamos una matriz de adyacencia vacía
    adj_matrix = []
    for i in range(num):
        new_row = []
        for j in range(num):
            new_row.append(0)
        adj_matrix.append(new_row)

    # Obtenemos todos los posibles valores de la columna Compuesto y los ordenamos
    labels = newdf[activityId].unique().tolist()
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
        new_session = row[caseId]
        new_label = row[activityId]
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
        
    if activityId == 'Compuesto':
        adj_matrix = myFilter(adj_matrix)    

        adj_matrix = removeCycles(adj_matrix)

        adj_matrix, labels = deleteZeros(adj_matrix,labels)
    else:
        adj_matrix[diccionario[last_label]][diccionario['END']] += 1

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
    
    # Cambiar de directorio
    os.chdir('../' + dot)
    
    # Guardar la imagen
    if activityId == 'Compuesto':
        getDotsCompound(image_name,labels,adj_matrix)
    else:
        getDotsProblem(image_name,labels,adj_matrix)
    comando = 'dot -Tpng ' + image_name + '.dot > ../' + graphs + '/' + image_name + '.png'
    subprocess.run(comando, shell=True)
    
    if activityId == 'Estado':
        # Cambiar de directorio
        os.chdir('../matrices_states_wc') 
        
        adj_matrix = remove(adj_matrix)
        
        # Escribir el vector y la matriz en el mismo archivo
        with open(file_name + '_wc' + '.txt', 'w') as f:
            label_text = ''
            for ele in labels:
                label_text = label_text + ele + ' '
            label_text = label_text + '\n'

            # Escribir el vector de las cabeceras en un archivo CSV
            f.write(label_text)

            # Escribir la matriz de adyacencia en un archivo CSV
            np.savetxt(f, adj_matrix, delimiter=' ', fmt='%d')
            
        # Cambiar de directorio
        os.chdir('../' + dot)
        
        getDotsProblem(image_name,labels,adj_matrix)
        comando = 'dot -Tpng ' + image_name + '.dot > ../' + graphs + '_wc' + '/' + image_name + '.png'
        subprocess.run(comando, shell=True)
        
    # Cambiamos de directorio
    os.chdir('../' + matrices)
    
# Función para determinar OK o FAIL
def determinar_estado(row):
    if row['Milestone'] > 80:
        return row['Problem'] + ' OK'
    else:
        return row['Problem'] + ' FAIL'

parser = argparse.ArgumentParser()
parser.add_argument("archivo", help="Ruta del archivo a procesar")
args = parser.parse_args()

# Leemos el dataset
df = pd.read_csv('MaribelDataset15-21.csv')

# Eliminamos las columnas con valores NaN
df=df.dropna(axis=1)

# Ordenamos los valores según las sesiones
df.sort_values(by="Session")

# Creamos los identificadores problema-milestone
df['Compuesto'] = 'PROBLEM' + df.Problem.apply(str).replace({'P':''}, regex=True) + '-' + df.Milestone.apply(str)

# Creamos los identificadores Pn OK o Pn FAIL
# Aplicar la función a cada fila del DataFrame y crear la nueva columna
df['Estado'] = df.apply(lambda row: determinar_estado(row), axis=1)

# Listado de grupos
grupos = df['Grupo'].unique().tolist()

# Convertir la columna 'sTime' en un objeto datetime
df['sTime'] = pd.to_datetime(df['sTime'])

# Convertir la columna 'sTime' en un número entero
df['sTime'] = df['sTime'].astype(int)

# Abrir el archivo donde se guardarán los resultados obtenidos
with open('results.csv', mode='w', newline='') as file:
    # Escribo la cabecera
    writer = csv.writer(file, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    writer.writerow(['Grupo', 'DAG', 'Q1', 'Q2', 'Q3', 'Q4', 'ST'])
    
    # Cambiamos de directorio
    os.chdir('matrices')

    for grupo in grupos:
        # Filtrado del dataset
        newdf = df[(df.Grupo == grupo)]
        # Arreglar índice del dataframe
        newdf.reset_index(drop=True, inplace=True)

        process(newdf, grupo.replace(" ",""), grupo.replace(" ",""), 'matrices', 'graphs', 'dot', 'Compuesto', 'Session')
        
        
        os.chdir('../matrices_problems')
        
        process(newdf, grupo.replace(" ",""), grupo.replace(" ",""), 'matrices', 'graphs_problems', 'dot_problems', 'Problem', 'Grupo')
        
        os.chdir('../matrices_states')
        
        process(newdf, grupo.replace(" ",""), grupo.replace(" ",""), 'matrices', 'graphs_states', 'dot_states', 'Estado', 'Grupo')
        
        dag = subprocess.run(["./../../C++/DAG/pruebaDAG", "1", grupo.replace(" ","") + ".txt"], stdout=subprocess.PIPE)
        output = dag.stdout.decode('utf-8')
        gr = output.split(' ')[0]
        coef = output.split(' ')[1]
        
        dag = subprocess.run(["./../../C++/DAG/pruebaDAG", "2", grupo.replace(" ","") + ".txt"], stdout=subprocess.PIPE)
        output = dag.stdout.decode('utf-8')
        ST = output.split(' ')[1]

        # Cuartiles de tiempo
        # Obtener el año de una fila específica
        index = 0  # Índice de la fila que se desea obtener
        year = newdf.loc[index, 'Year']

        df_year = df.loc[(df['Year'] == year)].copy()
        # Dividir los valores de sTime en cuartiles
        df_year.loc[:, 'quartiles'] = pd.qcut(df_year['sTime'], q=4)

        quartiles = df_year['quartiles'].unique().tolist()
        
        df_quartile_1 = newdf[newdf['sTime'] <= quartiles[0].right]
        process(df_quartile_1, grupo.replace(" ","") + "Q1", grupo.replace(" ","") + "Q1", 'matrices', 'time_graphs', 'time_dot', 'Compuesto', 'Session')
        
        dagQ1 = subprocess.run(["./../../C++/DAG/pruebaDAG", "1", grupo.replace(" ","") + "Q1.txt"], stdout=subprocess.PIPE)
        outputQ1 = dagQ1.stdout.decode('utf-8')
        if len(outputQ1.split(' '))>1:
            coefQ1 = outputQ1.split(' ')[1]
            
        df_2 = newdf[(newdf['sTime'] > quartiles[1].left) & (newdf['sTime'] <= quartiles[1].right)]
        
        if df_2.size == 0:
            coefQ2 = coefQ1
        else:
            df_quartile_2 = newdf[newdf['sTime'] <= quartiles[1].right]
            process(df_quartile_2, grupo.replace(" ","") + "Q2", grupo.replace(" ","") + "Q2", 'matrices', 'time_graphs', 'time_dot', 'Compuesto', 'Session')

            dagQ2 = subprocess.run(["./../../C++/DAG/pruebaDAG", "1", grupo.replace(" ","") + "Q2.txt"], stdout=subprocess.PIPE)
            outputQ2 = dagQ2.stdout.decode('utf-8')
            if len(outputQ2.split(' '))>1:
                coefQ2 = outputQ2.split(' ')[1]
        
        df_3 = newdf[(newdf['sTime'] > quartiles[2].left) & (newdf['sTime'] <= quartiles[2].right)]
        
        if df_3.size == 0:
            coefQ3 = coefQ2
        else:
            df_quartile_3 = newdf[newdf['sTime'] <= quartiles[2].right]
            process(df_quartile_3, grupo.replace(" ","") + "Q3", grupo.replace(" ","") + "Q3", 'matrices', 'time_graphs', 'time_dot', 'Compuesto', 'Session')

            dagQ3 = subprocess.run(["./../../C++/DAG/pruebaDAG", "1", grupo.replace(" ","") + "Q3.txt"], stdout=subprocess.PIPE)
            outputQ3 = dagQ3.stdout.decode('utf-8')
            if len(outputQ3.split(' '))>1:
                coefQ3 = outputQ3.split(' ')[1]
                
        df_4 = newdf[newdf['sTime'] > quartiles[3].left]
        
        if df_4.size == 0:
            coefQ4 = coefQ3
        else:
            df_quartile_4 = newdf
            process(df_quartile_4, grupo.replace(" ","") + "Q4", grupo.replace(" ","") + "Q4", 'matrices', 'time_graphs', 'time_dot', 'Compuesto', 'Session')

            dagQ4 = subprocess.run(["./../../C++/DAG/pruebaDAG", "1", grupo.replace(" ","") + "Q4.txt"], stdout=subprocess.PIPE)
            outputQ4 = dagQ4.stdout.decode('utf-8')
            if len(outputQ4.split(' '))>1:
                coefQ4 = outputQ4.split(' ')[1]
        
        writer.writerow([gr, coef, coefQ1, coefQ2, coefQ3, coefQ4, ST])
        
        # Cambiamos de directorio
        os.chdir('../matrices')