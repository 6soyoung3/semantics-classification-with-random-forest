#include <string>
using std::string;
#include <iostream>
using std::cout;
// #include <EmoForest.cpp>

string LoadFileName = "satimage.txt"; // Data txt file
string Path;                          // = "C:\Users\강지원\Desktop";
int numberofData = 4435;
const int featureDim = 36;   // number of features ex) if subjects: 1 = maths, 2 = english ...
int numberofLabel = 0; // or classes
int maxDepth = 15;     // levels of the tree
int numberTrees = 100;
int candidatenum = 500;                // select the threshold and the feature dimension randomly then split
double CountofeachLabel[];             // number of each label
double thresholdvalues[featureDim][2]; // array that holds max and min threshold of each label

int minvalofLabel = 999999;
int maxvalofLabel = -999999;

int main()
{
    int totalNodeNum = 2 ^ (maxDepth + 1) - 1;
    double dimension[numberofData][featureDim];

    for (int i = 0; i < numberofData; ++i)
    {
        for (int j = 0; j < featureDim; ++j)
        {
            dimension[i][j] = 0; // Assign everything as 0
            // this will hold the featurn values of each data, each label Next Next
        }
    }

    //Label array
    int label[numberofData]; // nth data's label/class

    // Read/Load the data from the file
    cout << "Data Load!\n";

    dimension = loadthedatafile(dimension, label);
    thresholdvalues = minMaxValues(dimension);

    cout << "Tree growing start!\n";

    // Learning
    for (int treeindex = 0; treeindex < numberTrees; ++treeindex)
    {
        // initial
        // Saves where data is saved, in which node
        int arrival[numberofData];

        // Nodes stated array 0: missing node, 1: Split node, 2: Leaf node
        int nodelists[totalNodeNum];

        // Leaf node info
        double LeafStatistic[totalNodeNum][numberofLabel];

        // Split node info
        double SplitStatistic[totalNodeNum][2]; // feature and its threshold

        for (int i = 0; i < totalNodeNum; ++i)
        {
            nodelists[i] = 0; // init all as missing nodes
        }

        for (int i = 0; i < numberofData; ++i)
        {
            arrival[i] = 0; // all at root node
        }

        split(treeindex, arrival, dimension, label, numberTrees, maxDepth, candidatenum, SplitStatistic, LeafStatistic, nodelists);
    }

    cout << "File Saving!\n";

    saveTreeset();

    cout << "File Saving Complete!\n";

    return 0;
}