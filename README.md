# Scalaで行列実装
Scalaで行列計算を実装しました。
ただし、モチベーションとしては、
型クラスの理解と並列処理の実装(つまりScalaの練習)だったので、アダマール積
(つまり要素ごとの積)までしかやっていません。

僕の環境では4スレッドくらいに分けるのが一番効率的みたいです。

## これからやりたいこと(たぶんしばらくやらない)

- コメントアウトをして`test` すると、なぞエラー

    ```aidl
    Unexpected New(TypeTree(class ValueMtrx2)/bababax11.matcala.matrix.ValueMtrx2) reached GenBCode.
    ```
    
    が出るので、それをなくしたい(`console`では動く)

- 行列積の実装

- 割り算、累乗の計算のできる`Numeric`に代わる型クラスの実装