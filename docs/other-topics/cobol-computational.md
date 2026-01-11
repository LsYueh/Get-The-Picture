# `COMP-3` 轉換規則

|  Sign  | Trailing byte |
| ---- | :--: |
|-Dca `Positive` | x'0F' |
|-Dcb/-Dci/-Dcm/-Dcr `Positive` | x'0C' |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Negative` | x'0D' |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Unsigned` | x'0F' |
|-Dcv `Unsigned` | x'0C' |

<br>

## Difference between COMP and COMP-3

|  COMP  | COMP-3 |
| :----: | :----: |
| It represents the data in pure binary form. | It represents the data in packed decimal form. |
| Can use only `9` and `S` in PIC Clause. | Ccan use `9` , `S` , `V` in PIC Clause. |
| COMP usage stores the data in `half word` or in `full word`, depending on the size of the data. | COMP3 usage stores `1 digit` in `half byte (i.e. 4 bits)` and a separate `1 bit` is reserved for the sign, which is stored at the right side of the data. |
| The memory to be occupied by the data according to the length is predefined i.e. : <br> • S9(01) - S9(04) : 16 bits (2 bytes) <br> • S9(05) - S9(09) :  32 bits (4 bytes) <br> • S9(10) - S9(18) :  64 bits (8 bytes) | The memory to be occupied by the data is defined by the following formula: <br> • (length of variable + 1)/2 bytes. <br> <br> Example : The memory occupied by S9(3) is: <br> (3+1)/2 i.e. 2 bytes. |
| COMP does not occupy extra space to store sign. | In COMP3 sign in compulsorily stored at right side and thus it occupies an extra space. |

Ref. [Difference between COMP and COMP3](https://www.geeksforgeeks.org/cobol/difference-between-comp-and-comp3/)

<br><br>
