#include <string>
using std::string;

string LoadFileName = "satimage.txt"; // Data txt file
string Path                           // = "C:\Users\강지원\Desktop";
    int numberofData = 4435;
int featureDim = 36;                          // number of features ex) if subjects: 1 = maths, 2 = english ...
int numberofLabel = 0;                        // or classes
int maxDepth = 15;                            // levels of the tree
int numberTrees = 100 int candidatenum = 500; // select the threshold and the feature dimension randomly then split
double CountofeachLabel[];                    // number of each label
double thresholdvalues[featureDim - 1, 1];    // array that holds max and min threshold of each label

int minvalofLabel = 999999;
int maxvalofLabel = -999999;

Sub Main() int totalNodeNum = CInt(2 ^ (maxDepth + 1) - 1);

double
    dimension[][] = Double(numberofData - 1)();
{
}
For i = 0 To numberofData - 1 dimension(i) = New Double(featureDim - 1) {}
For j = 0 To featureDim - 1 dimension(i)(j) = 0 'Assign everything as 0; this will hold the featurn values of each data, each label Next Next

        'Label array Dim label() = New Integer(numberofData - 1)
{
}
'nth data' s label / class

        'Read/Load the data from the file Console.WriteLine("Data Load!") dimension = loadthedatafile(dimension, label) thresholdvalues = minMaxValues(dimension)

        'Console.WriteLine("The first threshold min: " & thresholdvalues(0, 1) & " max: " & thresholdvalues(0, 0))

                     Console.WriteLine("Tree growing start!")

        'Learning For treeindex = 0 To numberTrees - 1
            'initial
            'Saves where data is saved, in which node Dim arrival(numberofData - 1) As Integer

            'Nodes stated array 0: missing node, 1: Split node, 2: Leaf node Dim nodelists = New Integer(totalNodeNum - 1)
{
}

'Leaf node info Dim LeafStatistic()() = New Double(totalNodeNum - 1)() {}
'every leaf node, label proportion

            'Split node info Dim SplitStatistic()() = New Double(totalNodeNum - 1)()
{
}
'node parameters

    For i = 0 To totalNodeNum - 1 nodelists(i) = 0 'init all as missing nodes Next

    For i = 0 To numberofData - 1 arrival(i) = 0 'all at root node Next

    For i = 0 To totalNodeNum - 1 SplitStatistic(i) = New Double(1)
{
}
'feature and its threshold Next

    For i = 0 To totalNodeNum - 1 LeafStatistic(i) = New Double(numberofLabel - 1)
{
}
Next

    split(treeindex, arrival, dimension, label, numberTrees, maxDepth, candidatenum, SplitStatistic, LeafStatistic, nodelists)

        Next

    Console.WriteLine("File Saving!")

        saveTreeset()

            Console.WriteLine("File Saving complete!")

                Console.ReadKey()

                    End Sub

    Function loadthedatafile(dimension As Double()(), label As Integer()) As Double()()

        Dim filename As String = Path & "\" & LoadFileName
        'Console.WriteLine(filename) Dim filereader As IO.StreamReader = New IO.StreamReader(filename) Dim line As String = "" Dim leftposition As Integer Dim rightposition As Integer

    Dim position As Integer = 0

    For i = 0 To numberofData - 1

            'Console.WriteLine("Reading data in the " & i + 1 & "-th Line") line = filereader.ReadLine

                                Dim templabel As Integer

                                While line.Substring(position, 1)<> " " position
    += 1 End While

    templabel = CInt(Left(line, position))

                    If minvalofLabel
                > templabel Then
                minvalofLabel = templabel End If
                                If maxvalofLabel < templabel Then
                                maxvalofLabel = templabel End If

    Next

    numberofLabel = maxvalofLabel - minvalofLabel + 1

                    CountofeachLabel = New Double(numberofLabel - 1)
{
}

        filereader = New IO.StreamReader(filename)

        position = 0

        For i = 0 To numberofData - 1 '*** is it possible to combine those two For loops into a single For loops
            'Console.WriteLine("Reading data in the " & i + 1 & "-th Line")
            line = filereader.ReadLine

            While line.Substring(position, 1) <> " "
                position += 1
            End While

            Dim templabel As Integer = CInt(Left(line, position))

            label(i) = templabel - minvalofLabel '*** so that label(0) = 0, label(1) = 1 ...

            CountofeachLabel(templabel - minvalofLabel) += 1.0

            For lineposition = 0 To line.Length - 1 'saving the feature and feature values
                If line(lineposition) = ":" Then
                    leftposition = 1 'Init every time
                    rightposition = 1
                    While line(lineposition - leftposition) <> " "
                        leftposition += 1
                    End While
                    While line(lineposition + rightposition) <> " "
                        rightposition += 1
                    End While
                    dimension(i)(CInt(Mid(line, lineposition - leftposition + 2, leftposition - 1)) - 1) = CDbl(Mid(line, lineposition + 2, rightposition - 1)) '***
                End If
            Next
        Next

        filereader.Close()

        Return dimension

    End Function

    Sub split(treeindex As Integer, arrival() As Integer, data()() As Double, label() As Integer, numberoftree As Integer, maxDepth As Integer, candidatenum As Integer, nodeparameter()() As Double, LeafStatistic()() As Double, nodelists() As Integer)

        Dim randomsample As Integer
        Dim score As Double
        Dim best_randomsample As Integer
        Dim best_score As Double
        Dim best_threshold As Double
        Static generator As System.Random = New System.Random

        For depth = 1 To maxDepth '12
            'nodenumber and nodeindex ***
            For nodenumbers = 1 To 2 ^ (depth - 1) '1 to 1, 1 to 2, 1 to 4 ---
                If countSample(arrival, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1)) > 1 Then 'otherwise no need to split

                    'Console.WriteLine(treeindex & "th tree:" & depth & "th depth" & nodenumbers & "th node split!")

                    nodelists(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1)) = 1 '*** at depth 1: 0, at depth 2: 1,2 --- up to 2^11 -1 + 2^11 - 1

                    'Candidate Sampling: (Feature dim., Threshold).

                    Randomize()

                    randomsample = generator.Next(0, featureDim)

                    Randomize()

                    Dim threshold As Double = generator.NextDouble 'to sample within the range of min/max

                    threshold = threshold * (thresholdvalues(randomsample, 0) - thresholdvalues(randomsample, 1)) + thresholdvalues(randomsample, 1) 'finding the threshold in the given range of min/max

                    'Init the first candidate as the best one.

                    Dim output(,) As Double = leftorright(arrival, data, label, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), randomsample, threshold)

                    Dim normalised(1, numberofLabel - 1) As Double

                    Dim sumleft As Double = 0.0
                    Dim sumright As Double = 0.0

                    For l = 0 To numberofLabel - 1
                        sumleft = sumleft + output(0, l)
                        sumright = sumright + output(1, l)
                    Next

                    'For l = 0 To numberofLabel - 1
                    'sumleft += output(0, l)
                    'sumright += output(1, l)
                    'Next

                    For l = 0 To numberofLabel - 1
                        If sumleft <> 0 Then
                            normalised(0, l) = output(0, l) / sumleft
                        Else
                            normalised(0, l) = 0
                        End If
                        If sumright <> 0 Then
                            normalised(1, l) = output(1, l) / sumright
                        Else
                            normalised(1, l) = 0
                        End If
                    Next

                    score = 0

                    For l = 0 To numberofLabel - 1
                        'Console.WriteLine("current score: " & score)
                        score = score + normalised(0, l) * normalised(0, l) * sumleft / (sumleft + sumright)
                        score = score + normalised(1, l) * normalised(1, l) * sumright / (sumleft + sumright)
                    Next

                    'Console.WriteLine("0-th:" & score)
                    best_randomsample = randomsample
                    best_score = score
                    best_threshold = threshold

                    'init the first sample as the best sample
                    'Loop for the remaining candidates

                    For m = 1 To candidatenum - 1

                        Randomize()

                        randomsample = generator.Next(0, featureDim)

                        Randomize()

                        threshold = generator.NextDouble()
                        threshold = threshold * (thresholdvalues(randomsample, 0) - thresholdvalues(randomsample, 1)) + thresholdvalues(randomsample, 1)

                        'Console.WriteLine(thresholdvalues(randomsample, 1) & "," & thresholdvalues(randomsample, 0) & "," & threshold)

                        output = leftorright(arrival, data, label, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), randomsample, threshold)

                        For l = 0 To numberofLabel - 1
                            normalised(0, l) = output(0, l) / (output(0, l) + output(1, l)) 'to return as probability
                            normalised(1, l) = output(1, l) / (output(0, l) + output(1, l))
                        Next

                        sumleft = 0.0
                        sumright = 0.0

                        For l = 0 To numberofLabel - 1
                            sumleft = sumleft + output(0, l)
                            sumright = sumright + output(1, l)
                        Next

                        For l = 0 To numberofLabel - 1
                            If sumleft <> 0 Then
                                normalised(0, l) = output(0, l) / sumleft
                            Else
                                normalised(0, l) = 0
                            End If
                            If sumright <> 0 Then
                                normalised(1, l) = output(1, l) / sumright
                            Else
                                normalised(1, l) = 0
                            End If
                        Next

                        score = 0 'score calculation / entrophy

                        For l = 0 To numberofLabel - 1
                            'Console.WriteLine("current score: " & score)
                            score = score + normalised(0, l) * normalised(0, l) * sumleft / (sumleft + sumright)
                            score = score + normalised(1, l) * normalised(1, l) * sumright / (sumleft + sumright)
                        Next

                        'Console.WriteLine(m & "-th:" & score)
                        '***

                        If score > best_score And score < 6 Then
                            best_score = score
                            best_randomsample = randomsample
                            best_threshold = threshold
                        End If

                    Next

                    'Console.WriteLine("selected:" & best_score)
                    'Save the best one as final parameter for the node.
                    nodeparameter(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1))(0) = best_randomsample
                    nodeparameter(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1))(1) = best_threshold

                    'split the tree with the best parameters and assign the node indexes into arrival array
                    leftorright_actual(arrival, data, label, depth, CInt(nodenumbers), CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), best_randomsample, best_threshold)
                    '*************************** nodenumber? nodeindex?

                End If
            Next
        Next

        Console.WriteLine("Making Leaf node!")
        For noOfData = 0 To numberofData - 1
            nodelists(arrival(noOfData)) = 2 '2 - leaf node
        Next

        '2. leaf node만드는 것 구현. leaf node는 현재 도달한 샘플로부터 normalize해서 label distribution 만들어서 저장.

        For nodenumbers = 0 To 2 ^ (maxDepth + 1) - 1 - 1 '2 ^ (depth - 1) - 1 + nodenumbers - 1
            If nodelists(CInt(nodenumbers)) = 2 Then
                makeLeaf(arrival, label, CInt(nodenumbers), LeafStatistic)
            End If
        Next

        saveSplitfile(nodeparameter, treeindex)
        saveLeafstatistics(LeafStatistic, treeindex)
        saveNodelists(nodelists, treeindex)
        saveArrival(arrival, treeindex)

    End Sub
    Function leftorright(arrival() As Integer, data()() As Double, label As Integer(), nodeindex As Integer, random_sample As Integer, threshold As Double) As Double(,)

        Dim output(1, numberofLabel - 1) As Double
        For i = 0 To numberofLabel - 1
            output(0, i) = 0
            output(1, i) = 0
        Next
        For dataposition = 0 To numberofData - 1
            'Console.WriteLine("The data is in this node" & arrival(dataposition) & " and the node index is " & nodeindex)
            If arrival(dataposition) = nodeindex Then 'the data position should be in that node

                'Console.WriteLine("Comparing data between: " & data(dataposition)(random_sample) & " Threshold: " & threshold)

                If data(dataposition)(random_sample) < threshold Then
                    output(0, label(dataposition)) += 1.0 / CountofeachLabel(label(dataposition)) 'weight
                    'weights are given as the number of samples in a sinlge label/class varies. to give a balance, put a weight
                Else
                    output(1, label(dataposition)) += 1.0 / CountofeachLabel(label(dataposition))
                End If
            End If
        Next

        Return output

    End Function
    'nums of samples arrived in that node
    Function countSample(arrival() As Integer, nodeindex As Integer) As Integer

        Dim count = 0

        For i = 0 To numberofData - 1
            If arrival(i) = nodeindex Then
                count = count + 1
            End If
        Next

        'Console.WriteLine("number of data in this node: " & count)

        Return count

    End Function

    Sub leftorright_actual(arrival() As Integer, data()() As Double, label As Integer(), depth As Integer, nodenumber As Integer, nodeindex As Integer, random_sample As Integer, threshold As Double)

        For dataposition = 0 To numberofData - 1
            If arrival(dataposition) = nodeindex Then
                If data(dataposition)(random_sample) < threshold Then 'which data has gone to left/right
                    arrival(dataposition) = CInt(2 ^ (depth) - 1 + (nodenumber - 1) * 2)
                Else
                    arrival(dataposition) = CInt(2 ^ (depth) - 1 + (nodenumber - 1) * 2 + 1)
                End If
            End If
        Next

    End Sub

    Sub makeLeaf(arrival() As Integer, label() As Integer, nodeindex As Integer, LeafStatistic()() As Double)

        '***
        Dim count(numberofLabel - 1) As Double

        For i = 0 To numberofData - 1
            If arrival(i) = nodeindex Then 'If arrival(dataposition) = nodeindex Then (only the data that has arrived to that node must be calculated)

                'Console.WriteLine(i & "th data:" & " label:" & label(i) & " location:" & arrival(i))
                '*** 
                count(label(i)) += 1.0 / CountofeachLabel(label(i)) 'count(the data's label) += 1/total number of the each label
                '각 label 이 proportionally 얼만큼 있는지 알기 위해 1/전체 label 갯수 를 더함

            End If
        Next

        Dim countsum As Double = 0

        For i = 0 To numberofLabel - 1
            countsum = countsum + CDbl(count(i)) '***
        Next

        For i = 0 To numberofLabel - 1
            LeafStatistic(nodeindex)(i) = count(i) / countsum 'Distribution of each label in a single leaf node
        Next

    End Sub
    Sub saveSplitfile(split()() As Double, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Split.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            filewriter.WriteLine(split(i)(0))
            filewriter.WriteLine(split(i)(1))
        Next

        filewriter.Close()

    End Sub
    Sub saveLeafstatistics(leafstatistics()() As Double, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Leaf.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            For j = 0 To numberofLabel - 1
                filewriter.WriteLine(leafstatistics(i)(j))
            Next
        Next

        filewriter.Close()
    End Sub
    Sub saveNodelists(nodelists() As Integer, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Nodelists.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            filewriter.WriteLine(nodelists(i))
        Next

        filewriter.Close()
    End Sub
    Sub saveArrival(arrival() As Integer, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_arrival.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To numberofData - 1
            filewriter.WriteLine(arrival(i))
        Next

        filewriter.Close()
    End Sub
    Sub saveTreeset()

        Dim filename As String = Path & "\Forest" & "\RFtreesinfo.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        filewriter.WriteLine(maxDepth)
        filewriter.WriteLine(numberTrees)
        filewriter.WriteLine(numberofLabel)
        filewriter.WriteLine(minvalofLabel)
        filewriter.WriteLine(maxvalofLabel)

        filewriter.Close()

    End Sub
    Function minMaxValues(dimension()() As Double) As Double(,)

        Dim max As Double = -99999999999999
        Dim min As Double = 99999999999999

        For i = 0 To featureDim - 1
            For j = 0 To numberofData - 1
                If dimension(j)(i) < min Then
                    min = dimension(j)(i)
                ElseIf dimension(j)(i) > max Then
                    max = dimension(j)(i)
                End If
            Next

            thresholdvalues(i, 1) = min
            thresholdvalues(i, 0) = max

        Next

        Return thresholdvalues

    End Function

End Module

Module Module1
    Dim LoadFileName As String = "satimage.txt" 'Data txt file
    Dim Path As String = "C:\Users\강지원\Desktop"
    Dim numberofData As Integer = 4435
    Dim featureDim As Integer = 36 'number of features ex) if subjects: 1 = maths, 2 = english ...
    Dim numberofLabel As Integer = 0 'or classes
    Dim maxDepth As Integer = 15 'levels of the tree
    Dim numberTrees As Integer = 100
    Dim candidatenum As Integer = 500 'select the threshold and the feature dimension randomly then split
    Dim CountofeachLabel() As Double 'number of each label
    Dim thresholdvalues(featureDim - 1, 1) As Double 'array that holds max and min threshold of each label

    Dim minvalofLabel As Integer = 999999
    Dim maxvalofLabel As Integer = -999999

    Sub Main()
        Dim totalNodeNum = CInt(2 ^ (maxDepth + 1) - 1)

        Dim dimension()() = New Double(numberofData - 1)()
        {
        }
        For i = 0 To numberofData - 1 dimension(i) = New Double(featureDim - 1) {}
        For j = 0 To featureDim - 1 dimension(i)(j) = 0 'Assign everything as 0; this will hold the featurn values of each data, each label Next Next

        'Label array Dim label() = New Integer(numberofData - 1)
        {
        }
        'nth data' s label / class

        'Read/Load the data from the file Console.WriteLine("Data Load!") dimension = loadthedatafile(dimension, label) thresholdvalues = minMaxValues(dimension)

        'Console.WriteLine("The first threshold min: " & thresholdvalues(0, 1) & " max: " & thresholdvalues(0, 0))

                             Console.WriteLine("Tree growing start!")

        'Learning For treeindex = 0 To numberTrees - 1
            'initial
            'Saves where data is saved, in which node Dim arrival(numberofData - 1) As Integer

            'Nodes stated array 0: missing node, 1: Split node, 2: Leaf node Dim nodelists = New Integer(totalNodeNum - 1)
        {
        }

        'Leaf node info Dim LeafStatistic()() = New Double(totalNodeNum - 1)() {}
        'every leaf node, label proportion

            'Split node info Dim SplitStatistic()() = New Double(totalNodeNum - 1)()
        {
        }
        'node parameters

            For i = 0 To totalNodeNum - 1 nodelists(i) = 0 'init all as missing nodes Next

            For i = 0 To numberofData - 1 arrival(i) = 0 'all at root node Next

            For i = 0 To totalNodeNum - 1 SplitStatistic(i) = New Double(1)
        {
        }
        'feature and its threshold Next

            For i = 0 To totalNodeNum - 1 LeafStatistic(i) = New Double(numberofLabel - 1)
        {
        }
        Next

            split(treeindex, arrival, dimension, label, numberTrees, maxDepth, candidatenum, SplitStatistic, LeafStatistic, nodelists)

                Next

            Console.WriteLine("File Saving!")

                saveTreeset()

                    Console.WriteLine("File Saving complete!")

                        Console.ReadKey()

                            End Sub

            Function loadthedatafile(dimension As Double()(), label As Integer()) As Double()()

                Dim filename As String = Path & "\" & LoadFileName
        'Console.WriteLine(filename) Dim filereader As IO.StreamReader = New IO.StreamReader(filename) Dim line As String = "" Dim leftposition As Integer Dim rightposition As Integer

            Dim position As Integer = 0

            For i = 0 To numberofData - 1

            'Console.WriteLine("Reading data in the " & i + 1 & "-th Line") line = filereader.ReadLine

                                        Dim templabel As Integer

                                        While line.Substring(position, 1)<> " " position
            += 1 End While

            templabel = CInt(Left(line, position))

                            If minvalofLabel
                        > templabel Then
                        minvalofLabel = templabel End If
                                        If maxvalofLabel < templabel Then
                                        maxvalofLabel = templabel End If

            Next

            numberofLabel = maxvalofLabel - minvalofLabel + 1

                            CountofeachLabel = New Double(numberofLabel - 1)
        {
        }

        filereader = New IO.StreamReader(filename)

        position = 0

        For i = 0 To numberofData - 1 '*** is it possible to combine those two For loops into a single For loops
            'Console.WriteLine("Reading data in the " & i + 1 & "-th Line")
            line = filereader.ReadLine

            While line.Substring(position, 1) <> " "
                position += 1
            End While

            Dim templabel As Integer = CInt(Left(line, position))

            label(i) = templabel - minvalofLabel '*** so that label(0) = 0, label(1) = 1 ...

            CountofeachLabel(templabel - minvalofLabel) += 1.0

            For lineposition = 0 To line.Length - 1 'saving the feature and feature values
                If line(lineposition) = ":" Then
                    leftposition = 1 'Init every time
                    rightposition = 1
                    While line(lineposition - leftposition) <> " "
                        leftposition += 1
                    End While
                    While line(lineposition + rightposition) <> " "
                        rightposition += 1
                    End While
                    dimension(i)(CInt(Mid(line, lineposition - leftposition + 2, leftposition - 1)) - 1) = CDbl(Mid(line, lineposition + 2, rightposition - 1)) '***
                End If
            Next
        Next

        filereader.Close()

        Return dimension

    End Function

    Sub split(treeindex As Integer, arrival() As Integer, data()() As Double, label() As Integer, numberoftree As Integer, maxDepth As Integer, candidatenum As Integer, nodeparameter()() As Double, LeafStatistic()() As Double, nodelists() As Integer)

        Dim randomsample As Integer
        Dim score As Double
        Dim best_randomsample As Integer
        Dim best_score As Double
        Dim best_threshold As Double
        Static generator As System.Random = New System.Random

        For depth = 1 To maxDepth '12
            'nodenumber and nodeindex ***
            For nodenumbers = 1 To 2 ^ (depth - 1) '1 to 1, 1 to 2, 1 to 4 ---
                If countSample(arrival, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1)) > 1 Then 'otherwise no need to split

                    'Console.WriteLine(treeindex & "th tree:" & depth & "th depth" & nodenumbers & "th node split!")

                    nodelists(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1)) = 1 '*** at depth 1: 0, at depth 2: 1,2 --- up to 2^11 -1 + 2^11 - 1

                    'Candidate Sampling: (Feature dim., Threshold).

                    Randomize()

                    randomsample = generator.Next(0, featureDim)

                    Randomize()

                    Dim threshold As Double = generator.NextDouble 'to sample within the range of min/max

                    threshold = threshold * (thresholdvalues(randomsample, 0) - thresholdvalues(randomsample, 1)) + thresholdvalues(randomsample, 1) 'finding the threshold in the given range of min/max

                    'Init the first candidate as the best one.

                    Dim output(,) As Double = leftorright(arrival, data, label, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), randomsample, threshold)

                    Dim normalised(1, numberofLabel - 1) As Double

                    Dim sumleft As Double = 0.0
                    Dim sumright As Double = 0.0

                    For l = 0 To numberofLabel - 1
                        sumleft = sumleft + output(0, l)
                        sumright = sumright + output(1, l)
                    Next

                    'For l = 0 To numberofLabel - 1
                    'sumleft += output(0, l)
                    'sumright += output(1, l)
                    'Next

                    For l = 0 To numberofLabel - 1
                        If sumleft <> 0 Then
                            normalised(0, l) = output(0, l) / sumleft
                        Else
                            normalised(0, l) = 0
                        End If
                        If sumright <> 0 Then
                            normalised(1, l) = output(1, l) / sumright
                        Else
                            normalised(1, l) = 0
                        End If
                    Next

                    score = 0

                    For l = 0 To numberofLabel - 1
                        'Console.WriteLine("current score: " & score)
                        score = score + normalised(0, l) * normalised(0, l) * sumleft / (sumleft + sumright)
                        score = score + normalised(1, l) * normalised(1, l) * sumright / (sumleft + sumright)
                    Next

                    'Console.WriteLine("0-th:" & score)
                    best_randomsample = randomsample
                    best_score = score
                    best_threshold = threshold

                    'init the first sample as the best sample
                    'Loop for the remaining candidates

                    For m = 1 To candidatenum - 1

                        Randomize()

                        randomsample = generator.Next(0, featureDim)

                        Randomize()

                        threshold = generator.NextDouble()
                        threshold = threshold * (thresholdvalues(randomsample, 0) - thresholdvalues(randomsample, 1)) + thresholdvalues(randomsample, 1)

                        'Console.WriteLine(thresholdvalues(randomsample, 1) & "," & thresholdvalues(randomsample, 0) & "," & threshold)

                        output = leftorright(arrival, data, label, CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), randomsample, threshold)

                        For l = 0 To numberofLabel - 1
                            normalised(0, l) = output(0, l) / (output(0, l) + output(1, l)) 'to return as probability
                            normalised(1, l) = output(1, l) / (output(0, l) + output(1, l))
                        Next

                        sumleft = 0.0
                        sumright = 0.0

                        For l = 0 To numberofLabel - 1
                            sumleft = sumleft + output(0, l)
                            sumright = sumright + output(1, l)
                        Next

                        For l = 0 To numberofLabel - 1
                            If sumleft <> 0 Then
                                normalised(0, l) = output(0, l) / sumleft
                            Else
                                normalised(0, l) = 0
                            End If
                            If sumright <> 0 Then
                                normalised(1, l) = output(1, l) / sumright
                            Else
                                normalised(1, l) = 0
                            End If
                        Next

                        score = 0 'score calculation / entrophy

                        For l = 0 To numberofLabel - 1
                            'Console.WriteLine("current score: " & score)
                            score = score + normalised(0, l) * normalised(0, l) * sumleft / (sumleft + sumright)
                            score = score + normalised(1, l) * normalised(1, l) * sumright / (sumleft + sumright)
                        Next

                        'Console.WriteLine(m & "-th:" & score)
                        '***

                        If score > best_score And score < 6 Then
                            best_score = score
                            best_randomsample = randomsample
                            best_threshold = threshold
                        End If

                    Next

                    'Console.WriteLine("selected:" & best_score)
                    'Save the best one as final parameter for the node.
                    nodeparameter(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1))(0) = best_randomsample
                    nodeparameter(CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1))(1) = best_threshold

                    'split the tree with the best parameters and assign the node indexes into arrival array
                    leftorright_actual(arrival, data, label, depth, CInt(nodenumbers), CInt(2 ^ (depth - 1) - 1 + nodenumbers - 1), best_randomsample, best_threshold)
                    '*************************** nodenumber? nodeindex?

                End If
            Next
        Next

        Console.WriteLine("Making Leaf node!")
        For noOfData = 0 To numberofData - 1
            nodelists(arrival(noOfData)) = 2 '2 - leaf node
        Next

        '2. leaf node만드는 것 구현. leaf node는 현재 도달한 샘플로부터 normalize해서 label distribution 만들어서 저장.

        For nodenumbers = 0 To 2 ^ (maxDepth + 1) - 1 - 1 '2 ^ (depth - 1) - 1 + nodenumbers - 1
            If nodelists(CInt(nodenumbers)) = 2 Then
                makeLeaf(arrival, label, CInt(nodenumbers), LeafStatistic)
            End If
        Next

        saveSplitfile(nodeparameter, treeindex)
        saveLeafstatistics(LeafStatistic, treeindex)
        saveNodelists(nodelists, treeindex)
        saveArrival(arrival, treeindex)

    End Sub
    Function leftorright(arrival() As Integer, data()() As Double, label As Integer(), nodeindex As Integer, random_sample As Integer, threshold As Double) As Double(,)

        Dim output(1, numberofLabel - 1) As Double
        For i = 0 To numberofLabel - 1
            output(0, i) = 0
            output(1, i) = 0
        Next
        For dataposition = 0 To numberofData - 1
            'Console.WriteLine("The data is in this node" & arrival(dataposition) & " and the node index is " & nodeindex)
            If arrival(dataposition) = nodeindex Then 'the data position should be in that node

                'Console.WriteLine("Comparing data between: " & data(dataposition)(random_sample) & " Threshold: " & threshold)

                If data(dataposition)(random_sample) < threshold Then
                    output(0, label(dataposition)) += 1.0 / CountofeachLabel(label(dataposition)) 'weight
                    'weights are given as the number of samples in a sinlge label/class varies. to give a balance, put a weight
                Else
                    output(1, label(dataposition)) += 1.0 / CountofeachLabel(label(dataposition))
                End If
            End If
        Next

        Return output

    End Function
    'nums of samples arrived in that node
    Function countSample(arrival() As Integer, nodeindex As Integer) As Integer

        Dim count = 0

        For i = 0 To numberofData - 1
            If arrival(i) = nodeindex Then
                count = count + 1
            End If
        Next

        'Console.WriteLine("number of data in this node: " & count)

        Return count

    End Function

    Sub leftorright_actual(arrival() As Integer, data()() As Double, label As Integer(), depth As Integer, nodenumber As Integer, nodeindex As Integer, random_sample As Integer, threshold As Double)

        For dataposition = 0 To numberofData - 1
            If arrival(dataposition) = nodeindex Then
                If data(dataposition)(random_sample) < threshold Then 'which data has gone to left/right
                    arrival(dataposition) = CInt(2 ^ (depth) - 1 + (nodenumber - 1) * 2)
                Else
                    arrival(dataposition) = CInt(2 ^ (depth) - 1 + (nodenumber - 1) * 2 + 1)
                End If
            End If
        Next

    End Sub

    Sub makeLeaf(arrival() As Integer, label() As Integer, nodeindex As Integer, LeafStatistic()() As Double)

        '***
        Dim count(numberofLabel - 1) As Double

        For i = 0 To numberofData - 1
            If arrival(i) = nodeindex Then 'If arrival(dataposition) = nodeindex Then (only the data that has arrived to that node must be calculated)

                'Console.WriteLine(i & "th data:" & " label:" & label(i) & " location:" & arrival(i))
                '*** 
                count(label(i)) += 1.0 / CountofeachLabel(label(i)) 'count(the data's label) += 1/total number of the each label
                '각 label 이 proportionally 얼만큼 있는지 알기 위해 1/전체 label 갯수 를 더함

            End If
        Next

        Dim countsum As Double = 0

        For i = 0 To numberofLabel - 1
            countsum = countsum + CDbl(count(i)) '***
        Next

        For i = 0 To numberofLabel - 1
            LeafStatistic(nodeindex)(i) = count(i) / countsum 'Distribution of each label in a single leaf node
        Next

    End Sub
    Sub saveSplitfile(split()() As Double, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Split.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            filewriter.WriteLine(split(i)(0))
            filewriter.WriteLine(split(i)(1))
        Next

        filewriter.Close()

    End Sub
    Sub saveLeafstatistics(leafstatistics()() As Double, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Leaf.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            For j = 0 To numberofLabel - 1
                filewriter.WriteLine(leafstatistics(i)(j))
            Next
        Next

        filewriter.Close()
    End Sub
    Sub saveNodelists(nodelists() As Integer, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_Nodelists.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To CInt(2 ^ (maxDepth + 1) - 1 - 1)
            filewriter.WriteLine(nodelists(i))
        Next

        filewriter.Close()
    End Sub
    Sub saveArrival(arrival() As Integer, treeIndex As Integer)
        Dim filename As String = Path & "\Forest" & "\" & treeIndex & "_arrival.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        For i = 0 To numberofData - 1
            filewriter.WriteLine(arrival(i))
        Next

        filewriter.Close()
    End Sub
    Sub saveTreeset()

        Dim filename As String = Path & "\Forest" & "\RFtreesinfo.txt"
        Dim filewriter As IO.StreamWriter = New IO.StreamWriter(filename, False)

        filewriter.WriteLine(maxDepth)
        filewriter.WriteLine(numberTrees)
        filewriter.WriteLine(numberofLabel)
        filewriter.WriteLine(minvalofLabel)
        filewriter.WriteLine(maxvalofLabel)

        filewriter.Close()

    End Sub
    Function minMaxValues(dimension()() As Double) As Double(,)

        Dim max As Double = -99999999999999
        Dim min As Double = 99999999999999

        For i = 0 To featureDim - 1
            For j = 0 To numberofData - 1
                If dimension(j)(i) < min Then
                    min = dimension(j)(i)
                ElseIf dimension(j)(i) > max Then
                    max = dimension(j)(i)
                End If
            Next

            thresholdvalues(i, 1) = min
            thresholdvalues(i, 0) = max

        Next

        Return thresholdvalues

    End Function
