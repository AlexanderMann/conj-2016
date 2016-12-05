# conj-2016

This repo was used to prepare the talk given by Alex Mann at Cognitect's 2016 Conj Conference. It includes a standard implementation of tSNE, examples of data rendered this way, a novel implementation of interop between Clojure and Python, a number of datasets which can be rendered into Clojure objects, and some examples of generatives testing.

# Credit
I want to start by citing the sources that helped me get this far. This list is by no means exhaustive as there are many blogs and whitepapers I consumed where the information remains and the name has fled.

## tSNE Groundwork
- [Original whitepaper](https://lvdmaaten.github.io/publications/papers/MachLearn_2012.pdf) by Hinton and van der Maaten
- Laurens van der Maaten's [tSNE resource website](https://lvdmaaten.github.io/tsne/)
- Joseph Turian's [modifications/code for tSNE](https://github.com/turian/textSNE)

## SENNA
- [Original whitepaper](http://ronan.collobert.com/pub/matos/2011_nlp_jmlr.pdf) detailing architecture of SENNA by Collobert and Weston
- [SENNA website](http://ronan.collobert.com/senna/)

## Datasets
I lifted datasets from the following places:
- MNIST from Turian's github repo (link above)
- 130000 Word embeddings from Collobert's SENNA site download (link above)
- Places from hiiamrohit's [countries-states-cities-database github repo](https://github.com/hiiamrohit/Countries-States-Cities-database)
- 3000 most common words were copy and pasted from [http://www.ef.com/english-resources/english-vocabulary/top-3000-words/](http://www.ef.com/english-resources/english-vocabulary/top-3000-words/)

# Expected Use
## Tests

```bash
lein test
```

## nREPL
I got sick of starting a headless repl, so the following will start a session at port 54321.

```
lein nrepl
```

## Rendering SVGs
There are examples of SVG rendering presented in the `core` namespace in the comments below. The gist is though, to run data through `tSNE`, then pipe it into `spit-svg`. Pretty straightforward!

# Example Renderings
## Collobert and Weston word embeddings of the english language's top 3000 words

![Alt text](https://rawgithub.com/AlexanderMann/conj-2016/master/renderings/dali.words-small.svg)

## The MNIST dataset

![Alt text](https://rawgithub.com/AlexanderMann/conj-2016/master/renderings/dali.mnist-1.svg)
