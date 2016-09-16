#### 0.2.2 - 2016-09-09
* Add logging to each SCPI request as the Visa level now uses bytes arrays

#### 0.2.1 - 2016-08-10
* Move to Endorphin.Core.SCPI-based IO for code reusability, and to make the IO
  much less opaque.  Users may now call the Endorphin.Core.SCPI functions
  directly on the instrument to write arbitrary SCPI commands to it.

#### 0.1.1 - 2016-08-01
* Update to newest version of Endorphin.Core to fix incorrect units of measure
  declarations.

#### 0.1.0 - 2016-07-01
* Initial open-source release
