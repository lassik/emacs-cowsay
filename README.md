# Cowsay in Emacs
This is a workalike of the popular [cowsay](https://en.wikipedia.org/wiki/Cowsay "Link to English Wikipedia") amusement program that
runs directly in Emacs and does not require any external programs.

This port is not written by the original author of `cowsay`, but
can load cartoon characters from the same files as the original and you can add custom cows (see [link](https://en.wikipedia.org/wiki/Cowsay "Repo") to repo with a lot of `.cow` files)!

## Commands
Here Commands, before run use `cowsay-load-cows` for load cows, this is required, if you hasn't cows, then clone anything repo with cows, and add source's path to `cowsay-directories`: ([example](https://github.com/paulkaefer/cowsay-files)), this is required:

  * `cowsay-string` read string from minibuffer and in new buffer see `cowsay` result with user's string
  * `cowsay-region` read region, and with this region view `cowsay` result in new buffer
  * `cowsay-replace-region` read region and replace its with `cowsay` result
  
## Customization
Here all variables, which you can change:

  * `cowsay-directories` is list of directories in which put `.cow` files, defaults to "/usr/local/share/cows" and "/usr/share/cowsay/cows"
  * `cowsay-preferred-cows` is list of names of cows, yea cows has names
  * `cowsay-eyes` is 2-character string to use as the cow's eyes.
  Example of customization:
  ```elisp
(setq cowsay-eyes "..")
  ```
  * `cowsay-tongue`is 2-character string to use as the cow's tongue
    Example of customization:
  ```elisp
  (setq cowsay-eyes "|]")
  ```
