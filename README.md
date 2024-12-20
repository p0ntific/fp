### ЗАДАЧИ ПО ФУНКЦИОНАЛЬНОМУ ПРОГРАММИРОВАНИЮ НА HASKELL

1. К лекциям 3-4 (задачи на натуральные числа, на строки и вещественные числа)
Задания для самостоятельной работы
Во всех нижеследующих заданиях требуется написать программу для решения поставленной задачи на языке Haskell.
Задание 1. Написать программу для нахождения n-го члена последовательности, заданной следующей рекуррентной формулой.
a0 = 1; a1 = 2;
an = 3 * an-1 – 2 * an-2 + 1 при n = 2, 3,...
Имейте в виду, что прямое программирование данной формулы «как есть» приводит к крайне неэффективной программе!
Задание 2. Совершенным числом называется натуральное число, равное сумме всех своих делителей, включая единицу, но исключая само это число. Так, например, число 28 – совершенное, поскольку 28 = 1 + 2 + 4 + 7 + 14. 
Написать программу для нахождения первых n совершенных чисел.
Задание 3. Близнецами называется пара натуральных чисел, каждое из которых равно сумме делителей другого числа. Так, например, числа 220 и 284 – близнецы (проверьте!). Написать программу для нахождения первых n пар близнецов.
Задание 4. В заданном списке строк найти самую длинную строку.
Задание 5. По заданному списку строк построить список строк, в котором содержатся те же строки, что и в исходном списке, но каждая вторая строка выброшена из списка (то есть в списке останутся только строки с нечетными номерами), а в каждой из оставшихся строк каждый второй символ также выброшен. 
Так, например, если аргументом программы является список строк
["Близнецами", "называется", "пара", "натуральных", "чисел"],
то результатом работы должен быть список строк ["Бинцм", "пр", "чсл"]
Задание 6. По заданному списку строк построить список строк, в котором содержатся те же строки, что и в исходном списке, но выброшены строки, содержащие хотя бы одну цифру. Проверить, является ли некоторый символ c цифрой, можно с помощью вызова стандартной
функции Haskell Char.isDigit.
Задание 7. Заданный числовой список разбить на подсписки из возрастающих подпоследовательностей максимальной длины рядом стоящих чисел. Так, например, если исходный список состоял из чисел  [2, 7, 10, 8, 3, 4, 9, 1, 2, 0, 8, 3, 2, 5], то результатом работы программы должен быть следующий список списков: [[2, 7, 10], [8], [3, 4, 9], [1, 2], [0, 8], [3],  [2, 5]].
Задание 8. Для заданного вещественного числа x найти сумму числового ряда с общим членом  un = xn / (n!+1). Суммирование производить, пока очередной член ряда не окажется по абсолютной величине меньше заданного числа.

2. К лекциям 7-8 	Высшие функции (2 задачи на операции с деревьями и 2 задачи на операции с графами)
Задания для самостоятельной работы
Задание 1. Дерево задано с помощью следующего описания структуры данных. 
data Tree a = Node a [Tree a]
то есть дерево представляет собой корневой узел, содержащий некоторое значение произвольного типа a и список поддеревьев. Написать функцию, проверяющую, что дерево является пирамидой, то есть значение в каждом из его узлов меньше, значений, хранящихся в поддеревьях этого узла.
Пирамида

Задание 2. В условиях предыдущего задания написать функцию, которая проверяет, верно ли, что значения, хранящиеся в корнях поддеревьев каждого узла, упорядочены в списке поддеревьев по возрастанию.
Задание 3. Структура графа задана списками смежности номеров вершин, то есть списком, элементами которого являются пары, состоящие из номера вершины и списка вершин, инцидентных ей. 
type Graph = [(Int, [Int])]
Написать функцию, которая выдает длину кратчайшего маршрута между двумя заданными вершинами. Длиной считать количество вершин, встретившихся в данном маршруте. Если маршрута между вершинами не существует, то функция должна выдавать ноль.
Задание 4. В условиях предыдущей задачи написать функцию, которая выдает кратчайший маршрут (в виде списка номеров вершин), соединяющий две заданные вершины в графе. Если такого маршрута не существует, то функция должна выдавать пустой список. Если номера первой и последней вершины совпадают, то функция выдаст список из единственной вершины.
Задание 5. Неориентированный граф с конечным числом вершин представлен парой из количества вершин и функции типа Int -> Int -> Bool, которая выдает True, если аргументы представляют собой номера вершин, соединенных ребром. Функция заведомо коммутативная, то есть если вершина a связана с вершиной b, то и вершина b связана с вершиной a.
type Graph = (Int, Int -> Int -> Bool)
Написать функцию, которая проверяет, является ли граф двудольным.
Задание 6. В условиях предыдущей задачи написать функцию, которая по номерам двух вершин, соединенных некоторым ребром, проверяет, является ли это ребро мостом в графе.

3. 1. К лекциям 10-11 		26-03-2024
Декларативный стиль
Упражнения
	•	В лекции встретилось много полезных стандартных функций, потренируйтесь с ними в интерпретаторе. Вызывайте их с различными значениями, экспериментируйте.
	•	Попробуйте определить функции из предыдущих глав в чисто композиционном стиле.
	•	Посмотрите на те функции, которые прошли и попробуйте переписать их определения шиворот на выворот. Если видите, что элемент написан композиционном стиле, перепишите его в декларативном, и наоборот. Получившиеся функции могут показаться монстрами, но это упражнение может помочь вам в закреплении новых конструкций и почувствовать сильные и слабые стороны того или иного стиля.
	•	Определите модуль, который будет вычислять площади простых фигур: треугольника, окружности, прямоугольника, трапеции. Помните, что фигуры могут задаваться различными способами.


Ленивые вычисления
Задания для самостоятельной работы
Задание 1. Бесконечная упорядоченная последовательность целых чисел без повторений составлена из всех квадратов, кубов и факториалов натуральных чисел. Составить программу для вычисления n-го члена этой последовательности.
Задание 2. Найти первые несколько простых чисел вида 2n+1.
Задание 3. Составить (бесконечный) список частичных сумм ряда, представляющего собой разложение числа e, полученное подстановкой единицы в ряд Тейлора для экспоненты.
Задание 4. Бесконечная пирамида (см. задание 4 данной темы) содержит в узлах все последовательные нечетные числа. Составить программу для построения такой пирамиды. Какие числа будут содержаться на первых четырех уровнях этой пирамиды после добавления в нее первых пяти четных чисел? Добавление можно производить в пирамиду с помощью алгоритма «протаскивания».

