using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

namespace Garden
{
    static class RandomExtensions
    {
        public static float NextFloat(this Random random) =>
            (float)random.NextDouble();
    }

    static class MathF
    {
        public static float Sin(float angle) => (float)Math.Sin(angle);
        public static float Cos(float angle) => (float)Math.Cos(angle);
        public static float Unit(float value) => MathHelper.Clamp(value, 0, 1);
    }

    class DrawingContext
    {
        static readonly VertexPositionColorTexture[] unitSquare = new[] {
                new VertexPositionColorTexture(
                    new Vector3(-0.5f, -0.5f, 0),
                    Color.White,
                    new Vector2(-1, -1)
                ),
                new VertexPositionColorTexture(
                    new Vector3(0.5f, -0.5f, 0),
                    Color.White,
                    new Vector2(1, -1)
                ),
                new VertexPositionColorTexture(
                    new Vector3(0.5f, 0.5f, 0),
                    Color.White,
                    new Vector2(1, 1)
                ),
                new VertexPositionColorTexture(
                    new Vector3(-0.5f, -0.5f, 0),
                    Color.White,
                    new Vector2(-1, -1)
                ),
                new VertexPositionColorTexture(
                    new Vector3(0.5f, 0.5f, 0),
                    Color.White,
                    new Vector2(1, 1)
                ),
                new VertexPositionColorTexture(
                    new Vector3(-0.5f, 0.5f, 0),
                    Color.White,
                    new Vector2(-1, 1)
                ),
            };

        public DrawingContext(GraphicsDevice device, Effect shapesEffect)
        {
            this.device = device;
            this.shapesEffect = shapesEffect;
            this.worldMatrix = this.shapesEffect.Parameters["World"];
            this.viewMatrix = this.shapesEffect.Parameters["View"];
            this.projectionMatrix = this.shapesEffect.Parameters["Projection"];
            this.circleTechnique = this.shapesEffect.Techniques["DrawCircles"];
            this.squareTechnique = this.shapesEffect.Techniques["DrawSquares"];
        }

        readonly GraphicsDevice device;
        readonly Effect shapesEffect;
        readonly EffectParameter worldMatrix;
        readonly EffectParameter viewMatrix;
        readonly EffectParameter projectionMatrix;
        readonly EffectTechnique circleTechnique;
        readonly EffectTechnique squareTechnique;

        public Matrix View
        {
            get { return this.viewMatrix.GetValueMatrix(); }
            set { this.viewMatrix.SetValue(value); }
        }

        public Matrix Projection
        {
            get { return this.projectionMatrix.GetValueMatrix(); }
            set { this.projectionMatrix.SetValue(value); }
        }

        public void DrawCircle(HSBColor idColor, Vector3 position, float radius)
        {
            // TODO: Batch/cache.
            var rgba = idColor.ToColor();
            var temp = (VertexPositionColorTexture[])unitSquare.Clone();
            for (int i = 0; i < temp.Length; i++)
            {
                temp[i].Color = rgba;
            }

            Matrix world =
                Matrix.CreateScale(radius * 2) *
                Matrix.CreateTranslation(position);
            this.worldMatrix.SetValue(world);
            this.shapesEffect.CurrentTechnique = this.circleTechnique;
            foreach (EffectPass pass in this.shapesEffect.CurrentTechnique.Passes)
            {
                pass.Apply();
                this.device.DrawUserPrimitives(
                    PrimitiveType.TriangleList, temp, 0, 2);
            }
        }

        public void DrawElipse(
            HSBColor idColor, Vector3 position, float radiusW, float radiusH)
        {
            // TODO: Batch/cache.
            var rgba = idColor.ToColor();
            var temp = (VertexPositionColorTexture[])unitSquare.Clone();
            for (int i = 0; i < temp.Length; i++)
            {
                temp[i].Color = rgba;
            }

            Matrix world =
                Matrix.CreateScale(radiusW * 2, radiusH * 2, 1) *
                Matrix.CreateTranslation(position);
            this.worldMatrix.SetValue(world);
            this.shapesEffect.CurrentTechnique = this.circleTechnique;
            foreach (EffectPass pass in this.shapesEffect.CurrentTechnique.Passes)
            {
                pass.Apply();
                this.device.DrawUserPrimitives(
                    PrimitiveType.TriangleList, temp, 0, 2);
            }
        }

        public void DrawSquare(
            HSBColor idColor,
            Matrix world,
            Vector3 pt0,
            Vector3 pt1,
            Vector3 pt2,
            Vector3 pt3)
        {
            var rgba = idColor.ToColor();
            var temp = (VertexPositionColorTexture[])unitSquare.Clone();
            temp[0].Position = pt0;
            temp[0].Color = rgba;
            temp[1].Position = pt1;
            temp[1].Color = rgba;
            temp[2].Position = pt2;
            temp[2].Color = rgba;

            temp[3].Position = pt0;
            temp[3].Color = rgba;
            temp[4].Position = pt2;
            temp[4].Color = rgba;
            temp[5].Position = pt3;
            temp[5].Color = rgba;

            this.worldMatrix.SetValue(world);
            this.shapesEffect.CurrentTechnique = this.squareTechnique;
            foreach (EffectPass pass in this.shapesEffect.CurrentTechnique.Passes)
            {
                pass.Apply();
                this.device.DrawUserPrimitives(
                    PrimitiveType.TriangleList, temp, 0, 2);
            }
        }
    }

    // This code is based on the code originally written by Kate Compton
    // (@GalaxyKate), at http://www.galaxykate.com/apps/Prototypes/LTrees/
    class Node
    {
        public static Random random = new Random();

        const int ANGLE_SKEW = 0;
        const int BUSHINESS = 1;
        const int WIGGLE = 2;
        const int VARIATION = 3;
        const int SHRINKAGE = 4;
        const int HUE_START = 5;
        const int HUE_DIFF = 6;
        const int SATURATION = 7;
        const int LEAF_COUNT = 8;
        const int LEAF_ASPECT = 9;
        const int LEAF_SHAPE = 10;
        const int FLOWER_COUNT = 11;
        const int FLOWER_HUE = 12;
        const int FLOWER_SATURATION = 13;
        const int PETAL_ASPECT = 14;

        static int NextID = 0;

        readonly List<Node> children = new List<Node>();
        readonly float radius;
        readonly int depth;
        readonly float[] dna;
        readonly float baseAngle;
        readonly float branchLength;
        readonly HSBColor idColor;
        readonly Node parent;
        readonly int id;


        float angle;
        Vector3 position;

        public Node(Node parent, float childPct)
        {
            this.id = NextID++;
            this.parent = parent;

            if (parent != null)
            {
                this.depth = parent.depth + 1;
                this.dna = parent.dna;

                var skew = (float)Math.Pow(this.dna[ANGLE_SKEW] - .5f, 3);
                var spread = (1.5f * this.dna[BUSHINESS]);
                this.baseAngle =
                    parent.angle + spread * (childPct - .5f) + skew;
                this.baseAngle +=
                    this.dna[WIGGLE] * .1f * MathF.Sin(this.depth) *
                    this.depth;

                var mult = 15 - 12 * this.dna[BUSHINESS];
                this.branchLength = .7f * mult * parent.radius;

                this.branchLength *=
                    (1 + this.dna[VARIATION] * (random.NextFloat() - .5f));
                this.radius = parent.radius * (.6f + .3f * this.dna[SHRINKAGE]);

                this.position = polarOffset(
                    parent.position, this.branchLength, this.baseAngle);
            }

            this.angle = this.baseAngle;
            this.idColor = makeIDColor(this.dna, this.depth);
        }

        public Node(float[] dna, Vector3 pos, float angle, float radius)
        {
            this.dna = dna;
            this.position = pos;
            this.baseAngle = this.angle = angle;
            this.radius = radius;
            this.idColor = makeIDColor(this.dna, this.depth);
        }

        static HSBColor makeIDColor(float[] dna, float depth)
        {
            float h = 3 + dna[HUE_START] + (0.1f * dna[HUE_DIFF] * depth);
            float s =
                (.7f + .3f * dna[SATURATION] * MathF.Sin(depth)) -
                (dna[SATURATION] * depth * .08f);
            float b = .3f + .1f * depth;
            return new HSBColor(h % 1f, MathF.Unit(s), MathF.Unit(b));
        }

        public void Iterate()
        {
            if (children.Count == 0 && radius > 2)
            {
                int branches = 1;
                if (depth % 3 == 0)
                {
                    branches = 2;
                }
                for (float i = 0; i < branches; i++)
                {
                    float pct = (i + 0.5f) / branches;
                    this.children.Add(new Node(this, pct));
                }
            }
            else
            {
                foreach (Node child in this.children)
                {
                    child.Iterate();
                }
            }
        }

        public void Update(GameTime gameTime)
        {
            float elapsed = (float)gameTime.TotalGameTime.TotalSeconds;
            if (this.parent != null)
            {
                float angleOffset = .1f * (1.2f + this.depth) *
                    MathF.Sin(2 * elapsed + this.depth);
                angleOffset += 0.2f * MathF.Sin(this.id);

                this.angle = this.baseAngle + angleOffset;
                this.position = polarOffset(
                    this.parent.position, this.branchLength, this.angle);
            }

            foreach (Node child in this.children)
            {
                child.Update(gameTime);
            }
        }

        public void Draw(DrawingContext context)
        {
            foreach (var child in this.children)
            {
                Vector3 edge = child.position - position;
                float length = child.branchLength;
                float angle = (float)Math.Atan2(edge.Y, edge.X);

                Matrix world =
                    Matrix.CreateRotationZ(angle) *
                    Matrix.CreateTranslation(this.position);

                context.DrawSquare(
                    this.idColor,
                    world,
                    new Vector3(0, -this.radius, 0),
                    new Vector3(0, +this.radius, 0),
                    new Vector3(length, +child.radius, 0),
                    new Vector3(length, -child.radius, 0)
                );

                var leafCount = (float)Math.Floor(this.dna[LEAF_COUNT] * 5);
                for (var j = 0; j < (int)leafCount; j++)
                {
                    HSBColor leafColor = this.idColor.Alter(
                        shade: 0.3f * MathF.Sin(j + this.depth),
                        fade: -0.3f + 0.2f * MathF.Sin(j + this.depth));

                    world =
                        Matrix.CreateTranslation(length / leafCount, 0, 0) *
                        world;

                    var r0 = 15 * this.radius * (.3f + this.dna[LEAF_ASPECT]);
                    var r1 = r0 * (.7f * this.dna[LEAF_SHAPE] + .12f);
                    var theta = MathF.Sin(j * 3 + this.depth);
                    var dTheta = 1 / (.8f + 2 * this.dna[LEAF_ASPECT]);
                    var theta0 = theta - dTheta;
                    var theta1 = theta + dTheta;

                    context.DrawSquare(
                        leafColor,
                        world,
                        new Vector3(0, 0, 0),
                        new Vector3(
                            r1 * MathF.Cos(theta0),
                            r1 * MathF.Sin(theta),
                            0),
                        new Vector3(
                            r0 * MathF.Cos(theta),
                            r0 * MathF.Sin(theta),
                            0),
                        new Vector3(
                            r1 * MathF.Cos(theta1),
                            r1 * MathF.Sin(theta1),
                            0));
                }


                child.Draw(context);
            }

            context.DrawCircle(this.idColor, this.position, this.radius);
            if (this.children.Count == 0)
            {
                Matrix world =
                    Matrix.CreateRotationZ(this.angle) *
                    Matrix.CreateTranslation(this.position);

                var flowerCount = (float)Math.Round(8 * this.dna[FLOWER_COUNT]);
                var petalSize = 5 * this.radius;

                var aspect = .1f + .9f * this.dna[PETAL_ASPECT];
                var petalH = petalSize * aspect;
                var petalW = petalSize * (1 - aspect);
                for (var i = 0; i < flowerCount; i++)
                {
                    var flowerColor = new HSBColor(
                        (this.dna[FLOWER_HUE] * 1.2f + .9f) % 1f,
                        this.dna[FLOWER_SATURATION],
                        MathF.Unit(.9f + .3f * MathF.Sin(i * 3)),
                        .7f);

                    world =
                        Matrix.CreateRotationZ(MathHelper.TwoPi / flowerCount) *
                        world;

                    var flowerCenter = Vector3.Transform(
                        new Vector3(petalH * 1.5f, 0, 0), world);
                    context.DrawElipse(
                        flowerColor,
                        flowerCenter,
                        petalH,
                        petalW);
                }
            }
        }

        static Vector3 polarOffset(Vector3 vector, float r, float theta)
        {
            Vector3 result;
            result.X = vector.X + r * MathF.Cos(theta + MathHelper.Pi);
            result.Y = vector.Y + r * MathF.Sin(theta + MathHelper.Pi);
            result.Z = vector.Z;
            return result;
        }
    }

    class KateGame : Game
    {
        GraphicsDeviceManager graphics;

        SpriteFont font;
        BasicEffect effect;
        Effect shapesEffect;
        DrawingContext drawingContext;

        Node node;

        public KateGame()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "content";
        }

        protected override void LoadContent()
        {
            font = Content.Load<SpriteFont>("fonts/basic");
            effect = new BasicEffect(GraphicsDevice);

            shapesEffect = Content.Load<Effect>("shaders/shapes");
            shapesEffect.CurrentTechnique = shapesEffect.Techniques[0];

            drawingContext = new DrawingContext(GraphicsDevice, shapesEffect);
        }

        protected override void UnloadContent()
        {
            font = null;
            shapesEffect = null;
            drawingContext = null;
        }

        protected override void Update(GameTime gameTime)
        {
            if (Keyboard.GetState().IsKeyDown(Keys.Enter))
            {
                node = null;
            }
            if (Keyboard.GetState().IsKeyDown(Keys.Escape))
            {
                Exit();
            }
            if (node == null)
            {
                float[] dna = new float[20];
                for (int i = 0; i < dna.Length; i++)
                {
                    dna[i] = (float)Node.random.NextDouble();
                }
                node = Spawn();
            }
            node.Update(gameTime);
        }

        static Node Spawn()
        {
            float[] dna = new float[20];
            for (int i = 0; i < dna.Length; i++)
            {
                dna[i] = (float)Node.random.NextDouble();
            }
            var node = new Node(
                dna,
                Vector3.Zero,
                -MathHelper.PiOver2,
                5 + Node.random.NextFloat() * 4);
            for (int i = 0; i < 10; i++)
            {
                node.Iterate();
            }
            return node;
        }

        protected override void Draw(GameTime gameTime)
        {
            var background = new HSBColor(0.55f, 0.1f, 1f, 1);
            GraphicsDevice.Clear(background.ToColor());
            GraphicsDevice.RasterizerState = RasterizerState.CullNone;
            GraphicsDevice.BlendState = BlendState.AlphaBlend;

            Matrix view = Matrix.CreateLookAt(
                new Vector3(0, 0, -10),
                new Vector3(0, 0, 0),
                Vector3.UnitY) * Matrix.CreateScale(0.75f);
            Matrix projection = Matrix.CreateOrthographic(
                GraphicsDevice.Viewport.Width,
                GraphicsDevice.Viewport.Height,
                0.0f,
                100.0f);

            this.drawingContext.View = view;
            this.drawingContext.Projection = projection;

            this.node.Draw(this.drawingContext);
        }
    }
}
