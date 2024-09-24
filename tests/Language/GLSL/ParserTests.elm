module Language.GLSL.ParserTests exposing (suite)

import Expect
import Language.GLSL.Parser as P
import Language.GLSL.Syntax
    exposing
        ( Compound(..)
        , Declaration(..)
        , Expr(..)
        , ExternalDeclaration(..)
        , FullType(..)
        , FunctionIdentifier(..)
        , FunctionPrototype(..)
        , InitDeclarator(..)
        , InvariantOrType(..)
        , Parameters(..)
        , PrecisionQualifier(..)
        , Statement(..)
        , StorageQualifier(..)
        , TranslationUnit(..)
        , TypeQualifier(..)
        , TypeSpecifier(..)
        , TypeSpecifierNoPrecision(..)
        , TypeSpecifierNonArray(..)
        )
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Language.GLSL.Parser"
        [ Test.describe "Triangle"
            [ Test.test "vertexShader" <|
                \_ ->
                    P.parse """
                        attribute vec3 position;
                        attribute vec3 color;
                        uniform mat4 perspective;
                        varying vec3 vcolor;

                        void main () {
                            gl_Position = perspective * vec4(position, 1.0);
                            vcolor = color;
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (TranslationUnit
                                    [ Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Attribute)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "position" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Attribute)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "color" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Uniform)) (TypeSpec Nothing (TypeSpecNoPrecision Mat4 Nothing)))) [ InitDecl "perspective" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Varying)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "vcolor" Nothing Nothing ])
                                    , FunctionDefinition
                                        (FuncProt
                                            (FullType Nothing
                                                (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))
                                            )
                                            "main"
                                            []
                                        )
                                        (Compound
                                            [ ExpressionStatement
                                                (Just
                                                    (Equal (Variable "gl_Position")
                                                        (Mul (Variable "perspective")
                                                            (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing)))
                                                                (Params [ Variable "position", FloatConstant 1.0 ])
                                                            )
                                                        )
                                                    )
                                                )
                                            , ExpressionStatement (Just (Equal (Variable "vcolor") (Variable "color")))
                                            ]
                                        )
                                    ]
                                )
                            )
            , Test.test "fragmentShader" <|
                \_ ->
                    P.parse """
                        precision mediump float;
                        varying vec3 vcolor;

                        void main () {
                            gl_FragColor = vec4(vcolor, 1.0);
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (TranslationUnit
                                    [ Declaration (Precision MediumP (TypeSpecNoPrecision Float Nothing))
                                    , Declaration
                                        (InitDeclaration
                                            (TypeDeclarator
                                                (FullType (Just (TypeQualSto Varying))
                                                    (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing))
                                                )
                                            )
                                            [ InitDecl "vcolor" Nothing Nothing ]
                                        )
                                    , FunctionDefinition
                                        (FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))) "main" [])
                                        (Compound
                                            [ ExpressionStatement
                                                (Just
                                                    (Equal (Variable "gl_FragColor")
                                                        (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing)))
                                                            (Params [ Variable "vcolor", FloatConstant 1.0 ])
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    ]
                                )
                            )
            ]
        ]
