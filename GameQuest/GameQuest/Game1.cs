using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Myra;
using static Screens;

namespace GameQuest
{
    public class Game1 : Game
    {
        private GraphicsDeviceManager graphics;
        private SpriteBatch? spriteBatch;
        private ScreenManager? screenManager;

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
            this.screenManager?.Screen?.Dispose();
            this.screenManager = this.screenManager?.DoStep(e);
            this.screenManager?.Screen?.Initialise();
        }

        protected override void LoadContent()
        {
            this.spriteBatch = new SpriteBatch(GraphicsDevice);
            MyraEnvironment.Game = this;
            this.screenManager = new ScreenManager(ScreenJourney.ScreenJouney(this.OnScreenJourneyEvent));
            this.screenManager?.Screen?.Initialise();
        }

        protected override void Update(GameTime gameTime)
        {
            this.screenManager?.Screen?.OnUpdate(gameTime);
            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.Black);
            this.screenManager?.Screen?.OnRender();
            base.Draw(gameTime);
        }
    }
}