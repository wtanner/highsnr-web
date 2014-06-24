--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "random/*.html" $ do
        route   idRoute
	compile copyFileCompiler

    match "posts/*" $ version "raw" $ do
        route idRoute
	compile getResourceBody

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
		>>= loadAndApplyTemplate "templates/post.html" postCtx
		>>= loadAndApplyTemplate "templates/default.html" postCtx
            	>>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "wes.asc.txt" $ do
    	route idRoute
	compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Welcome to HighSNR"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
