# BB Une console colorée

```bash
dotnet add package ColoredConsole --version 1.0.0
```

dans son fichier :

```cs
using ColoredConsole;

ColorConsole.WriteLine($"Your grade average is {result:N1} _<(<)/°=".Magenta().OnWhite());
ColorConsole.WriteLine($"The highest grade is {maxGrade:N2} <(°-°)>".DarkBlue().OnRed());
ColorConsole.WriteLine($"The smallest grade is {minGrade:N2} __(..°>".Green());
```

<img src="assets/Screenshot2020-07-30at17.05.45.png" alt="Screenshot 2020-07-30 at 17.05.45" style="zoom:50%;" />
