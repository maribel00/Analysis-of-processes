/**
 * @file DAG.cpp
 * @author María Isabel Ruiz Martínez (you@domain.com)
 * @brief Implementation of the class DAG
 * @version 0.1
 * @date 2023-03-06
 * 
 * @copyright Copyright (c) 2023
 * 
 */

#include <fstream>
#include <sstream>
#include <iomanip> // Necesaria para imprimir las tablas
#include <numeric> // Necesaria para utilizar accumulate
#include <cmath> // Necesaria para utilizar sqrt
#include <algorithm> // Necesaria para utilizar la función sort
#include <eigen3/Eigen/Dense> // Necesaria para calcular determinantes
#include "DAG.h"

using namespace std;

DAG::DAG(string filename, int option) {
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "No se pudo abrir el archivo espeficado." << endl;
        file_correct = false;
    }
    else {
        file_correct = true;
        ifstream file(filename);
        string line;

        if (option == 2) {
            getline(file, line);

            while (getline(file, line)) {
                istringstream iss(line);

                vector<int> row;
                int num;
                while (iss >> num) {
                    row.push_back(num);
                }
                frequency.push_back(row);
            }
        }
        else {
            while (getline(file, line)) {
                istringstream iss(line);

                if (line.substr(0, 7) == "PROBLEM") { // Es una línea de encabezado
                    string elemento;
                    while (iss >> elemento)
                        header.push_back(elemento);
                } else { // Es una línea de datos
                    vector<int> row;
                    int num;
                    while (iss >> num) {
                        row.push_back(num);
                    }
                    frequency.push_back(row);
                }
            }

            if (this->frequency.size() > 0){

                this->frequency.pop_back();

                this->size = frequency[0].size(); // Obtenemos el número de columnas

                find_paths(this->size-2); // Obtenemos todos los caminos

                // Eliminar caminos con transiciones iniciales a un problema con un porcentaje > 20
                int i = 0;
                vector<int> good_paths;
                for (const auto& name : names){
                    if (split(name.front()).back().compare("20") != 0){
                        frequency[size-2].at(paths[i].front()) = 0;
                    }
                    else
                        good_paths.push_back(i);
                    ++i;
                }

                vector<vector<int>> new_paths;
                vector<vector<string>> new_names;

                for (const auto& ele : good_paths){
                    new_paths.push_back(paths[ele]);
                    new_names.push_back(names[ele]);
                }

                paths.clear();
                names.clear();

                for (const auto& path : new_paths)
                    paths.push_back(path);

                for (const auto& name : new_names)
                    names.push_back(name);

                remove_duplicates(); // Eliminamos caminos repetidos
            }
        }

        // Calculamos la matriz de probabilidades
        float sum = 0.0f;
        for (const auto& row : frequency){
            for (const auto& ele : row)
                sum += ele;
        }

        for (const auto& row : frequency){
            vector<float> prob;
            vector<float> zero;
            for (const auto& ele : row){
                prob.push_back(ele/sum);
                zero.push_back(0.0f);
            }
            degree_matrix.push_back(zero);
            probabilities.push_back(prob);
        }

        for (int i = 0; i < frequency.size(); i++) {
            int sum = 0;
            for (int j = 0; j < frequency[0].size(); j++) {
                sum += frequency[j][i] + frequency[i][j];
            }
            degree_matrix[i][i] += sum;
        }
    }
}

bool DAG::duplicates(vector<int> v){
    sort(v.begin(), v.end()); // Ordenar el vector

    // Comprobar duplicados
    bool duplicates = false;
    for (std::size_t i = 0; i < v.size() - 1; ++i) {
        if (v[i] == v[i+1]) {
            duplicates = true;
            break;
        }
    }

    return duplicates;
}

vector<string> DAG::split(string id) {
    int posInit = 0;
    int posFound = 0;
    string splitted;
    vector<string> results;
    
    posFound = id.find('M', posInit);
    posInit = posFound + 1;

    posFound = id.find('-', posInit);
    splitted = id.substr(posInit, posFound - posInit);
    posInit = posFound + 1;
    results.push_back(splitted);

    int posEnd = id.length();
    splitted = id.substr(posInit, posEnd);
    results.push_back(splitted);
    return results;
}

int DAG::num_problems(vector<string> path){
    int num_problems = 0;
    vector<string> old;

    if (path.size() >= 2){
        old = split(path.front());
        num_problems++;
        for (int i = 2; i < path.size(); ++i){
            vector<string> actual = split(path[i]);
            if (actual.front().compare(old.front()) != 0)
                num_problems++;
            old = split(path[i]);
        }
    }
    else
        num_problems = 1;

    return num_problems;
}

vector<int> DAG::find_index(int row){
    vector<int> index;
    int i = 0;
    for (const auto& list : paths) {
        if (list.size() > 0){
            if (list.back() == row)
                index.push_back(i);
        }
        ++i;
    }
    return index;
}

vector<int> DAG::find_columns(int row){
    vector<int> columns;
    for (int col = 0; col < size; ++col){
        if (frequency[row].at(col) > 0){
            columns.push_back(col);
        }
    }
    return columns;
}

void DAG::remove_duplicates() {
    sort(paths.begin(), paths.end()); // ordena el vector de vectores
    paths.erase(unique(paths.begin(), paths.end()), paths.end()); // elimina los elementos duplicados
    sort(names.begin(), names.end()); // ordena el vector de vectores
    names.erase(unique(names.begin(), names.end()), names.end()); // elimina los elementos duplicados
}

int DAG::find_paths(int row){

    if (row == (size-1)){ // Hemos llegado a END
        vector<int> index = find_index(row);
        if (index.size() > 0){
            for (const auto& ele : index){
                paths[ele].pop_back();
                names[ele].pop_back();
            }
        }
        return 0;
    }
    else if (row == (size-2)){ // Encontrar los nodos después de START
        vector<int> columns = find_columns(row);
        for (const auto& col : columns){
            vector<int> path;
            path.push_back(col);
            paths.push_back(path);
            string name = header[col];
            vector<string> aux;
            aux.push_back(name);
            names.push_back(aux);
            find_paths(col);
        }
    }
    else {
        vector<int> index = find_index(row);
        vector<int> columns = find_columns(row);

        if (columns.size() > 0){
            int col = columns.back();
 
            vector<vector<int>> old_paths;
            vector<vector<string>> old_names;

            for (const auto& ele : paths)
                old_paths.push_back(ele);

            for (const auto& ele : names)
                old_names.push_back(ele);

            for (const auto& ele : index){
                vector<int> new_path;
                vector<string> new_sequence;

                for (const auto& integer : paths[ele])
                    new_path.push_back(integer);
                for (const auto& n : names[ele])
                    new_sequence.push_back(n);

                new_path.push_back(col);
                new_sequence.push_back(header[col]);
                
                if (!duplicates(new_path)){
                    paths[ele].push_back(col);
                    names[ele].push_back(header[col]);
                }
                else {
                    frequency[row].at(col) = 0;
                }
            }

            columns.pop_back();

            find_paths(col);

            for (const auto& column : columns){
                for (const auto& ele : index){
                    vector<int> new_path;
                    vector<string> new_name;

                    for (const auto& integer : old_paths[ele])
                        new_path.push_back(integer);
                    for (const auto& n : old_names[ele])
                        new_name.push_back(n);

                    new_path.push_back(column);
                    new_name.push_back(header[column]);
                    
                    if (!duplicates(new_path)){
                        paths.push_back(new_path);
                        names.push_back(new_name);
                    }
                    else {
                        frequency[row].at(column) = 0;
                    }
                }
                find_paths(column);
            }
        }
    }
}

int DAG::floyd_warshall() {
    int n = frequency.size();
    vector<vector<int>> D(n, vector<int>(n));
    
    // Inicialización de la matriz D
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            D[i][j] = frequency[i][j];
        }
    }
    
    // Cálculo de la matriz de caminos más cortos D
    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (D[i][k] != numeric_limits<int>::max() && D[k][j] != numeric_limits<int>::max()) {
                    D[i][j] = min(D[i][j], D[i][k] + D[k][j]);
                }
            }
        }
    }
    
    // Cálculo de la separación del grafo
    int separation = numeric_limits<int>::max();
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (D[i][j] == numeric_limits<int>::max()) {
                continue;
            }
            int s = 0;
            for (int k = 0; k < n; k++) {
                if (D[i][k] != numeric_limits<int>::max() && D[k][j] != numeric_limits<int>::max()) {
                    s = max(s, D[i][k] + D[k][j]);
                }
            }
            separation = min(separation, s);
        }
    }
    
    return separation;
}

float DAG::get_coefficient(){

    float coefficient = 0.0f;

    // Cálculo de la media del número de problemas por camino
    vector<int> problems;
    for (const auto& name : names)
        problems.push_back(num_problems(name));

    if (names.size() == 0)
        coefficient = 3.0f;
    else {
        // Calcular la media
        float mean = static_cast<float>(std::accumulate(problems.begin(), problems.end(), 0)) / problems.size();

        // Normalizar
        mean = (mean - 1.0f)/(9.0f - 1.0f);

        coefficient += 0.2f * mean;

        // Cálculo de la desviación estándar de las frecuencias no nulas
        vector<int> nonzero;
        float mean_frequency = 0.0f;
        int n = 0;

        // Calcular la media de las frecuencias no nulas
        for (const auto& row : frequency){
            for (const auto& ele : row) {
                if (ele > 0){
                    nonzero.push_back(ele);
                    mean_frequency += ele;
                    n++;
                }
            }
        }

        mean_frequency /= n;

        // Calcular la suma de los cuadrados de las diferencias
        float sum_squares = 0.0f;
        
        for (int i = 0; i < n; i++)
            sum_squares += pow(nonzero[i]-mean_frequency, 2);

        // Dividir la suma de los cuadrados por el número de valores menos 1
        float variance = sum_squares / (n-1);

        // Calcular la raíz cuadrada de la varianza para obtener la desviación estándar
        float standard_deviation = sqrt(variance);

        coefficient += 0.8f * (float)(standard_deviation/mean_frequency);
    }

    return coefficient;
}

float DAG::get_spanning_trees(){
    Eigen::MatrixXf laplacian_matrix(frequency.size()-1, frequency.size()-1);
    for (int i = 1; i < frequency.size(); i++) {
        for (int j = 1; j < frequency.size(); j++)
            laplacian_matrix(i-1,j-1) = degree_matrix[i][j] - frequency[i][j];
    }

    return log10(laplacian_matrix.determinant());
}

float DAG::get_entropy(){
    vector<float> entropy;
    for (int i = 0; i < probabilities.size(); ++i){
        float I = 0.0f;
        for (int j = 0; j < probabilities[i].size(); ++j){
            if (probabilities[i][j] != 0)
                I -= probabilities[i][j]*log2(probabilities[i][j]);
        }
        entropy.push_back(I);
    }

    float mean = 0.0f;
    for (const auto& ele : entropy)
        mean += ele;
    mean = mean / entropy.size();

    float sum_squared_diff = 0.0;
    for (int i = 0; i < entropy.size(); i++) {
        float diff = entropy[i] - mean;
        sum_squared_diff += diff * diff;
    }

    float variance = sum_squared_diff / entropy.size();
    float stddev = sqrt(variance);

    return stddev;
}

float DAG::get_grade(){
    int size = frequency.size();
    int grade_2 = 0;

    for (int i = 0; i < size-2; ++i){
        int count = 0;
        for (int j = 0; j < frequency[i].size(); ++j) {
            if (frequency[i][j] != 0) {
                count++;
            }
        }
        if (count == 1)
            grade_2++;
    }

    return (float) grade_2/(size-2);
}

ostream& operator<<(ostream& ostr, const DAG& dag) {
    // Imprime los datos de ambas matrices
    ostr << endl;

    ostr << "FREQUENCY:" << endl;
    ostr << setfill('-') << setw(7 * dag.size + 1) << "" << endl;
    for (const auto& row : dag.frequency) {
        ostr << "|";
        for (const auto& cell : row) {
            ostr << setfill(' ') << setw(5) << cell << " |";
        }
        ostr << endl;
        ostr << setfill('-') << setw(7 * dag.size + 1) << "" << endl;
    }

    ostr << endl;
    ostr << endl;

    // Imprime la cabecera
    ostr << "HEADER:" << endl;
    for (const auto& cell : dag.header) {
        ostr << cell << " ";
    }
    ostr << endl;

    return ostr;
}
