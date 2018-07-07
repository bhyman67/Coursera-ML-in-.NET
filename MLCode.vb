
Imports System
Imports System.IO
Imports Accord.Math
Imports Accord.Controls
Imports System.Windows.Forms
Imports System.Data.SqlClient
Imports Accord.Math.Optimization
Imports Accord.Statistics.Analysis
Imports Accord.Statistics.Visualizations
Imports Accord.Statistics.Models.Regression
Imports Accord.Statistics.Models.Regression.Linear
Imports Accord.Statistics.Models.Regression.Fitting

Module Module1

	' +++++++ General Helper Functs/Code +++++++

	' Be sure to provide file directory path here. 
	Dim fileName As String = "...\outPutFile.txt"
	Dim sw As New StreamWriter(fileName)

	Sub makeScatterPlot(x() As Double, y() As Double)

		Dim theScaterplot As New Scatterplot()
		theScaterplot.Compute(x, y)
		theScaterplot.Title = "ex1data1"
		ScatterplotBox.Show(theScaterplot)

	End Sub

	Function Convert2Jagged(givenData As Double(,), rowCount As Integer, colCount As Integer)

		Dim thejaggedArr(rowCount - 1)() As Double
		For i = 0 To rowCount - 1
			ReDim thejaggedArr(i)(colCount - 1)
			For j = 0 To colCount - 1
				thejaggedArr(i)(j) = givenData(i, j)
			Next
		Next

		Convert2Jagged = thejaggedArr

	End Function

	' Will have funct that writes output to a .txt right here...

	' +++++++ Database Access Functs/Code +++++++

	Dim SqlServerInstance As String = "" ' name of SQL server instance goes here. I used a local SQL server instanced installed on my PC
	Dim Database As String = "MLSandbox"

	Function getCnxnString(ByVal MSSQLName As String, ByVal DB As String) As String

		Dim cnxnString As String

		Dim cnxnStringBlder As New SqlConnectionStringBuilder()
		With cnxnStringBlder
			.DataSource = MSSQLName
			.InitialCatalog = DB
			.IntegratedSecurity = True
		End With

		cnxnString = cnxnStringBlder.ConnectionString
		cnxnStringBlder = Nothing

		getCnxnString = cnxnString

	End Function

	Function GetVectorFromDB(ByVal SQL As String) As Double()

		Dim tempArray(0) As Double

		Using DBCnxn As New SqlConnection(getCnxnString(SqlServerInstance, Database))

			DBCnxn.Open()

			Dim cmd As New SqlCommand(SQL, DBCnxn)
			Dim reader As SqlDataReader = cmd.ExecuteReader
			Dim i As Integer = 0
			While reader.Read()
				If i <> 0 Then
					ReDim Preserve tempArray(i)
				End If
				tempArray(i) = reader(0)
				i += 1
			End While

			DBCnxn.Close()

		End Using

		GetVectorFromDB = tempArray

	End Function

	Function GetMatrixFromDB(ByVal sql As String)

		Using DBCnxn As New SqlConnection(getCnxnString(SqlServerInstance, Database))

			DBCnxn.Open()

			Using cmd As New SqlCommand(sql, DBCnxn)

				Dim reader As SqlDataReader = cmd.ExecuteReader
				Dim colCount As Integer = reader.FieldCount - 1
				Dim tempMatrix(colCount, 0) As Double
				Dim i As Integer = 0
				While reader.Read()

					If i <> 0 Then
						ReDim Preserve tempMatrix(colCount, i)
					End If
					For j = 0 To colCount

						tempMatrix(j, i) = reader(j)

					Next
					i += 1

				End While

				GetMatrixFromDB = tempMatrix.Transpose

			End Using

			DBCnxn.Close()

		End Using

	End Function

	' +++++++ Helper Functs/Code for Linear Regresion Lab +++++++

	Dim data(,) As Double = GetMatrixFromDB("select 1 as ones, x, y from Coursera_ML.ex1data1")
	Dim Len As Integer = data.GetColumn(0).Length ' would like to replace this code to just use a matrix dimension instead of a vector dim...
	Dim x(,) As Double = data.Get(0, Len, {0, 1})
	Dim y() As Double = data.GetColumn(2)

	Function CostFunction(Theta() As Double) As Double

		Dim vect() As Double = x.Dot(Theta).Add(y.Multiply(-1))
		Return (1 / (2 * Len)) * (vect.Dot(vect))

	End Function

	Function GradientFunction(Theta() As Double) ' need to indicate the data type returned just to make code clear

		Dim vect() As Double = x.Dot(Theta).Add(y.Multiply(-1))
		Dim matrix(,) As Double = x.Transpose().Multiply(2)
		Return matrix.Dot(vect).Multiply(1 / Len)

	End Function

	Function Predict(Hypothysis() As Double, Theta() As Double)

		Predict = Hypothysis.Dot(Theta)

	End Function

	' +++++++ Lab Subroutines +++++++

	' Linear regression problem (with one variable)
	'	-> The objective is to predict profits for a food truck in a city with population x. 
	'	-> Will implement linear regresion in mult ways
	Sub Lab1_LinearRegression()

		sw.WriteLine("		+++++ Lab1_LinearRegression +++++ " + Environment.NewLine)

		' Plot the data
		' makeScatterPlot(x.GetColumn(1), y) -> hold off on this for now....

		Dim gd As New GradientDescent

		Dim CostFunct As Func(Of Double(), Double) = AddressOf CostFunction
		Dim GradientFunct As Func(Of Double(), Double()) = AddressOf GradientFunction
		With gd
			.NumberOfVariables = 2
			.Function = CostFunct
			.Gradient = GradientFunct
			.Iterations = 1500
			.LearningRate = 0.01
		End With

		' Calculate the cost with initial parameters
		Dim initialParameters() As Double = {0, 0}
		sw.WriteLine("The cost with parameter vector set to zeros is: {0}", CostFunct(initialParameters))

		' Implement Gradient Descent algorithm
		gd.Minimize(initialParameters)

		Dim p As Double = 3.5
		sw.WriteLine("Predict y at x = {0}: {1}", p, Predict({1, p}, gd.Solution))
		p = 7.0
		sw.WriteLine("Predict y at x = {0}: {1}", p, Predict({1, p}, gd.Solution))
		p = 15.0
		sw.WriteLine("Predict y at x = {0}: {1}", p, Predict({1, p}, gd.Solution))

		sw.WriteLine("The cost with current soln is {0}", CostFunct(gd.Solution))

		Dim ols As New OrdinaryLeastSquares
		Dim regresion As SimpleLinearRegression = ols.Learn(x.GetColumn(1), y)

		sw.WriteLine(regresion.Transform(15))

	End Sub

	Sub Lab2_LogisticRegression()

		sw.WriteLine("		+++++ Lab2_LogisticRegression +++++ " + Environment.NewLine)

		Dim theMatrix As Double(,) = GetMatrixFromDB("SELECT x, y FROM Coursera_ML.ex2data1")
		Dim rowCount As Integer = theMatrix.GetLength(0)
		Dim colCount As Integer = theMatrix.GetLength(1)
		Dim Input As Double()() = Convert2Jagged(theMatrix, rowCount, colCount)
		Dim output As Double() = GetVectorFromDB("SELECT z FROM Coursera_ML.ex2data1")

		Dim log_reg_analysis As New LogisticRegressionAnalysis()
		log_reg_analysis.Learn(Input, output)

		sw.WriteLine(log_reg_analysis.Transform({45.0, 85.0}))

	End Sub

	Sub Main()

		' Call each subroutine for each lab
		Lab1_LinearRegression()
		Lab2_LogisticRegression()

		sw.Close()

		Console.WriteLine("Done")

	End Sub

End Module
