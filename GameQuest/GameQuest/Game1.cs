using Microsoft.FSharp.Core;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Myra;
using static Screens;

namespace GameQuest
{
    public class Game1 : Game
    {
        private GraphicsDeviceManager graphics;
        private SpriteBatch spriteBatch;
        private ScreenManager screenManager;
        private FSharpOption<IScreen> menu;

        public Game1()
        {
            this.graphics = new GraphicsDeviceManager(this);
            graphics.IsFullScreen = false;
            graphics.PreferredBackBufferWidth = 800;
            graphics.PreferredBackBufferHeight = 600;
            Content.RootDirectory = "Content";
            IsMouseVisible = true;
        }

        protected override void Initialize()
        {
            base.Initialize();
        }

        private void OnScreenJourneyEvent(Screens.ScreenJourneyEvent e)
        {
            if (this.menu != null)
            {
                this.menu.Value.Dispose();
            }

            this.menu = this.screenManager.DoStep(e);

            if (this.menu != null)
            {
                this.menu.Value.Initialise();
            }
        }

        protected override void LoadContent()
        {
            this.spriteBatch = new SpriteBatch(GraphicsDevice);

            MyraEnvironment.Game = this;

            this.screenManager = new ScreenManager(ScreenJourney.ScreenJouney(this.OnScreenJourneyEvent));

            this.menu = this.screenManager.DoStep(ScreenJourneyEvent.Initialise);

            if (this.menu != null)
            {
                this.menu.Value.Initialise();
            }
        }

        protected override void Update(GameTime gameTime)
        {
            if (this.menu != null)
            {
                this.menu.Value.OnUpdate(gameTime);
            }

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.Black);

            if (this.menu != null)
            {
                this.menu.Value.OnRender();
            }

            base.Draw(gameTime);
        }
    }
}