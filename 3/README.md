# Assignment 3

### Submission by
* Abhinav Singh 140101002
* Yash Pote 140101080

### Implement Haskell Functions for Basic Set Operations
We have implemented empty, union, intersection, subtraction, addition (minkowski sum) functions. As a subroutine of addition we have written operation function which performs the given operation for corresponding elements of two sets.

### Billing System for Fast Food Stall
The main function starts the billing system which does the following steps - getOrder, getItem (check if item exists), buyItem (check if item has enough quantity), takeItem (reduce item quantity from store), getBill (generates the bill).

### Generating Cipher text from Plaintext and Vice Versa
We implemented cipher and decipher functions using the symmetric cryptographic algorithm. The addChars and subChars functions implement wrap and unwrap.

Encryption:

* Input: text and key to generate the cipher.
* First character of text rotates by the amount specified in first character of key, second character rotates by amount specified in second character in key... This keeps repeating. This is done by adding up the ASCII values of the two character.
* If the resulting character is beyond the range of the English alphabet, wrap around by subtracting 26.
* When the key run outs its characters, it again starts with its first character and continue the same procedure.
* If the text contains any uppercase letter, do not change that, only encode the lower case letter.
* If the text contains a number, encode each digit by putting a special symbol in place of the digit (0=*, 1=`, 2=~, 3=!, 4=@, 5=#, 6=$, 7=%, 8=^, 9=&)
* x = a + b, where a, b are the ASCII values.
* x - 26 * k <= 122 where k needs to be found for wrapping.

Example:
Encrypt Bye90, key = iitg

* B is left unchanged
* y changes to z with key i, sum is 326 which wraps to 122
* e changes to f with key i, sum is 206 which wraps to 102
* 9 changes to &, corresponding to its index in symbol list
* 0 changes to *, corresponding to its index in symbol list

Decryption:

* Input: cipher and key to generate the text.
* First character of cipher unrotates by the amount specified in first character of key, second character unrotates by amount specified in second character in key... This keeps repeating. This is done by subtracting the ASCII values of the two character.
* The resulting character is unwrapped around by adding 26 till it is just smaller than maximum of English alphabet.
* When the key run outs its characters, it again starts with its first character and continue the same procedure.
* If the cipher contains any uppercase letter, do not change that, only encode the lower case letter.
* If the cipher contains a special symbol, decode it by putting a digit corresponding to (0=*, 1=`, 2=~, 3=!, 4=@, 5=#, 6=$, 7=%, 8=^, 9=&)

* x = a - b, where a, b are the ASCII values.
* x + 26 * k <= 122 where k needs to be found for unwrapping.

Example:
Decrypt Bzf&*, key = iitg

* B is left unchanged
* z changes to y with key i, sub is 17 which wraps to 121
* f changes to e with key i, sub is -3 which wraps to 101
* & changes to 9, corresponding to its index in symbol list
* \* changes to 0, corresponding to its index in symbol list
