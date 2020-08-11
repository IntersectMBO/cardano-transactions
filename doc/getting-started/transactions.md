## Payment

We call _Payment_ a simple UTxO transactions with no metadata, moving funds from a set of inputs to a set of outputs.

Payments are constructed from a small set of primitives, following the given state-machine:

![](https://mermaid.ink/img/eyJjb2RlIjoic3RhdGVEaWFncmFtXG5cdFsqXSAtLT4gQ29pblNlbGVjdGlvbjogZW1wdHlcbiAgICBDb2luU2VsZWN0aW9uIC0tPiBDb2luU2VsZWN0aW9uOiBhZGRJbnB1dFxuICAgIENvaW5TZWxlY3Rpb24gLS0-IENvaW5TZWxlY3Rpb246IGFkZE91dHB1dFxuICAgIENvaW5TZWxlY3Rpb24gLS0-IFRyYW5zYWN0aW9uOiBsb2NrXG4gICAgVHJhbnNhY3Rpb24gLS0-IFRyYW5zYWN0aW9uOiBzaWduV2l0aFxuICAgIFRyYW5zYWN0aW9uIC0tPiBbKl06IHNlcmlhbGl6ZSIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)

### Example (Haskell)

<details>
  <summary>Imports</summary>

```hs
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.UTxO.Transaction.Cardano.Byron
    ( fromBase16
    , fromBase58
    , mkInput
    , mkOutput
    , mkSignKey
    , testnetMagic
    )

import qualified Data.ByteString as BS
import qualified Data.UTxO.Transaction as Tx
```
</details>

<details>
  <summary>Constructing Inputs / Outputs</summary>

```hs
-- Say we want to construct a transaction from a known input to two
-- different addresses. Let's start by constructing the primitive types
-- for /Byron/ by using the smart-constructors from:
--
--   'Data.UTxO.Transaction.Cardano.Byron'

let (Just input0) = mkInput 0 =<< fromBase16
      "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7"

let Just key0 = mkSignKey =<< fromBase16
      "e0860dab46f13e74ab834142e8877b80bf22044cae8ebab7a21ed1b8dc00c155\
      \f6b78eee2a5bbd453ce7e7711b2964abb6a36837e475271f18ff36ae5fc8af73\
      \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb"

let oneAda = 1_000_000

let (Just output0) = mkOutput oneAda =<< fromBase58
      "2cWKMJemoBajc46Wu4Z7e6bG48myZWfB7Z6pD77L6PrJQWt9HZ3Yv7o8CYZTBMqHTPTkv"

let (Just output1) = mkOutput oneAda =<< fromBase58
      "2cWKMJemoBaiLiNB8QpHKjkQhnPdQSyxaLb8JJFUQYpiVzgEJE59fN7V7StqnyDuDjHYJ"
```
</details>

<details>
  <summary>Constructing The Transaction</summary>

```hs
-- Next, we can construct the transaction using the DSL provided by:
--
--   'Data.UTxO.Transaction#MkPayment'

let eitherTx = Tx.empty testnetMagic
      & Tx.addInput input0
      & Tx.addOutput output0
      & Tx.addOutput output1
      & Tx.lock
      & Tx.signWith key0
      & Tx.serialize
```
</details>

<details>
  <summary>Writing Binary Transaction to a File</summary>

```hs
-- Finally, let's export the binary transaction to a file, if we didn't screw
-- up the in the above example ^^"

case eitherTx of
  Left e ->
    fail $ show e
  Right bytes ->
    BS.writeFile "transaction.bin" bytes
```
</details>

### Example (CLI)

```shell
cardano-tx empty 764824073 \
  | cardano-tx add-input 0 3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7 \
  | cardano-tx add-output 42 Ae2tdPwUPEZETXfbQxKMkMJQY1MoHCBS7bkw6TmhLjRvi9LZh1uDnXy319f \
  | cardano-tx lock \
  | cardano-tx sign-with e0860dab46f13e74ab834142e8877b80bf22044cae8ebab7a21ed1b8dc00c155f6b78eee2a5bbd453ce7e7711b2964abb6a36837e475271f18ff36ae5fc8af73e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb \
  | cardano-tx serialize
```