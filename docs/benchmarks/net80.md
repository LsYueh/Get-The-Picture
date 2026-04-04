# Benchmark result (基準測試結果) - .NET 8.0

```bash
BenchmarkDotNet v0.15.8, Windows 11 (10.0.26200.8037/25H2/2025Update/HudsonValley2)
Intel Core i5-10400 CPU 2.90GHz, 1 CPU, 12 logical and 6 physical cores
.NET SDK 10.0.201
  [Host]     : .NET 8.0.25 (8.0.25, 8.0.2526.11203), X64 RyuJIT x86-64-v3
  DefaultJob : .NET 8.0.25 (8.0.25, 8.0.2526.11203), X64 RyuJIT x86-64-v3
```

> 1 µs = 1000 ns  

<br>

## Wrapper

| Method                | Mean      | Error     | StdDev    |
|---------------------- |----------:|----------:|----------:|
| Wrapper_Read_String   |  4.418 μs | 0.0341 μs | 0.0319 μs |
| Wrapper_Write_String  |  4.721 μs | 0.0365 μs | 0.0342 μs |
| Wrapper_Read_Integer  |  4.855 μs | 0.0118 μs | 0.0098 μs |
| Wrapper_Write_Integer |  6.079 μs | 0.0330 μs | 0.0292 μs |
| Wrapper_Read_Decimal  |  6.128 μs | 0.1216 μs | 0.1138 μs |
| Wrapper_Write_Decimal | 10.401 μs | 0.0678 μs | 0.0601 μs |

> ⚠️ T30 的資料內沒有進行 `COMP`，目前的 Wrapper 跑分算是 Best Case。  
> ⚠️ Wrapper 只做**單筆欄位**讀取。  

<br>

## DISPLAY

Integer: `PIC 9(18)` / `PIC S9(18)`   
Decimal: `PIC S9(5)V9(2)`  

| Method                       | Mean      | Error    | StdDev   |
|----------------------------- |----------:|---------:|---------:|
| Display_Read_Integer         |  84.32 ns | 0.245 ns | 0.217 ns |
| Display_Write_Integer        | 121.96 ns | 0.448 ns | 0.374 ns |
| Display_Read_Signed_Integer  |  96.40 ns | 0.441 ns | 0.391 ns |
| Display_Write_Signed_Integer | 195.94 ns | 0.762 ns | 0.676 ns |
| Display_Read_Decimal         |  99.01 ns | 0.807 ns | 0.755 ns |
| Display_Write_Decimal        | 222.29 ns | 1.676 ns | 1.568 ns |

<br>

## COMP-3

Integer: `PIC 9(18)` / `PIC S9(18)`   
Decimal: `PIC S9(5)V9(2)`  

| Method                     | Mean      | Error    | StdDev   |
|--------------------------- |----------:|---------:|---------:|
| Comp3_Read_Integer         |  88.36 ns | 0.208 ns | 0.162 ns |
| Comp3_Write_Integer        |  93.10 ns | 0.202 ns | 0.169 ns |
| Comp3_Read_Signed_Integer  |  85.45 ns | 0.385 ns | 0.342 ns |
| Comp3_Write_Signed_Integer |  93.29 ns | 0.311 ns | 0.275 ns |
| Comp3_Read_Decimal         |  97.51 ns | 0.236 ns | 0.209 ns |
| Comp3_Write_Decimal        | 150.38 ns | 0.618 ns | 0.548 ns |

<br>

## COMP-4 (BE)

Integer: `PIC 9(18)` / `PIC S9(18)`  

| Method                     | Mean      | Error    | StdDev   |
|--------------------------- |----------:|---------:|---------:|
| Comp4_Read_Integer         |  42.57 ns | 0.168 ns | 0.141 ns |
| Comp4_Write_Integer        | 132.29 ns | 0.349 ns | 0.291 ns |
| Comp4_Read_Signed_Integer  |  41.10 ns | 0.204 ns | 0.191 ns |
| Comp4_Write_Signed_Integer | 129.02 ns | 0.471 ns | 0.417 ns |

<br>

## COMP-5

Integer: `PIC S9(18)`  

| Method                 | Mean      | Error    | StdDev   |
|----------------------- |----------:|---------:|---------:|
| Comp5_Read_Integer_BE  |  47.72 ns | 0.166 ns | 0.139 ns |
| Comp5_Write_Integer_BE | 132.47 ns | 0.559 ns | 0.467 ns |
| Comp5_Read_Integer_LE  |  38.76 ns | 0.080 ns | 0.067 ns |
| Comp5_Write_Integer_LE | 132.59 ns | 0.547 ns | 0.485 ns |

<br>

## COMP-6

Integer: `PIC 9(18)`  

| Method              | Mean     | Error    | StdDev   |
|-------------------- |---------:|---------:|---------:|
| Comp6_Read_Integer  | 91.96 ns | 1.219 ns | 1.140 ns |
| Comp6_Write_Integer | 96.89 ns | 0.235 ns | 0.196 ns |

<br><br>
