#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

using namespace std;

int main() {
    string filename = "Dataset Fusionado - Compuesto.csv";
    ifstream file(filename);

    vector<vector<int>> frequency;
    vector<vector<int>> duration;
    vector<string> header;

    // Omitir la primera línea del archivo
    string line;
    getline(file, line);

    bool found_frequency = false;
    bool found_duration = false;

    while (getline(file, line)) {
        if (!found_frequency && line.find("Total Frequency") != string::npos) {
            getline(file, line);
            found_frequency = true;
            continue;
        } else if (found_frequency && line.find("Total Duration ms") != string::npos) {
            getline(file, line);
            found_duration = true;
            continue;
        }

        if (found_frequency && !found_duration) {
            vector<int> row;
            stringstream ss(line);
            string cell;
            getline(ss, cell, ','); // Ignorar el primer elemento de la fila
            header.push_back(cell);
            while (getline(ss, cell, ',')) {
                row.push_back(stoi(cell));
            }

            frequency.push_back(row);
        } else if (found_duration) {
            vector<int> row;
            stringstream ss(line);
            string cell;
            getline(ss, cell, ','); // Ignorar el primer elemento de la fila
            while (getline(ss, cell, ',')) {
                row.push_back(stoi(cell));
            }

            duration.push_back(row);
        }
    }

    // Imprime los datos de ambas matrices
    cout << "FREQUENCY:" << endl;
    for (const auto& row : frequency) {
        for (const auto& cell : row) {
            cout << cell << " ";
        }
        cout << endl;
    }

    cout << "DURATION:" << endl;
    for (const auto& row : duration) {
        for (const auto& cell : row) {
            cout << cell << " ";
        }
        cout << endl;
    }

    // Imprime la cabecera
    cout << "HEADER:" << endl;
    for (const auto& cell : header) {
        cout << cell << " ";
    }
    cout << endl;

    return 0;
}
